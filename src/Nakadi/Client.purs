module Nakadi.Client
 ( Env
 , NakadiResponse
 , getEventTypes
 , postEventType
 , getEventType
 , putEventType
 , deleteEventType
 , postEvents
 , postSubscription
 , streamSubscriptionEvents
 )
 where

import Prelude

import Affjax (Request, Response, ResponseFormatError, printResponseFormatError)
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, ask, runReaderT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldMap)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Options ((:=))
import Data.String (toLower)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, runAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error, throwException)
import Effect.Ref as Ref
import FlowId.Types (FlowId)
import Foreign (ForeignError, renderForeignError)
import Foreign.Object as Object
import Gzip.Gzip as Gzip
import Nakadi.Errors (E207, E400, E401, E403, E404, E409, E422, e207, e400, e401, e403, e404, e409, e422)
import Nakadi.Types (Cursor, CursorDistanceQuery, CursorDistanceResult, Event, EventType, EventTypeName(..), Partition, Problem, StreamParameters, Subscription, SubscriptionCursor, SubscriptionEventStreamBatch, SubscriptionId(..), XNakadiStreamId(..))
import Node.Encoding (Encoding(..))
import Node.HTTP.Client as HTTP
import Node.Stream (onDataString, pipe)
import Node.Stream as Stream
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

type Env r =
  { flowId  :: FlowId
  , baseUrl :: String
  , port    :: Int
  , token   :: Effect String
  | r
  }

type NakadiResponse r a = Either (Variant (unauthorized :: E401 | r)) a

request :: ∀ a m. MonadAff m => Request a -> m (Response (Either ResponseFormatError a))
request = liftAff <<< AX.request

baseHeaders :: ∀ r m . MonadAsk (Env r) m => MonadAff m => m (Array (Tuple String String))
baseHeaders = do
  env <- ask
  token <- liftEffect $ env.token
  pure [ Tuple "X-Flow-ID" (unwrap env.flowId)
       , Tuple "Authorization" ("Bearer " <> token)
       ]

baseRequest :: ∀ r m . MonadAsk (Env r) m => MonadAff m => Method -> String -> m (Request String)
baseRequest method path = do
  env <- ask
  headers <- baseHeaders <#> (map \(Tuple k v) -> RequestHeader k v)
  let port = show env.port
  pure $
    AX.defaultRequest
    { method = Left method
    , responseFormat = ResponseFormat.string
    , headers = headers
    , url = env.baseUrl <> ":" <> port <> path
    }

getRequest :: ∀ r m. MonadAsk (Env r) m => MonadAff m => String -> m (Request String)
getRequest = baseRequest GET

deleteRequest :: ∀ r m. MonadAsk (Env r) m => MonadAff m => String -> m (Request String)
deleteRequest = baseRequest DELETE

writeRequest :: ∀ a m r. WriteForeign a => MonadAsk (Env r) m => MonadAff m => Method -> String -> a -> m (Request String)
writeRequest method path content = do
  req <- baseRequest method path
  let contentString = writeJSON content -- # spy "Body to send"
  let body = Just (RequestBody.string contentString)
  let headers = req.headers <> [RequestHeader "Content-Type" "application/json"]
  pure $ req
    { content = body
    , headers = headers
    }

postRequest :: ∀ a m r . WriteForeign a => MonadAsk (Env r) m => MonadAff m => String -> a -> m (Request String)
postRequest = writeRequest POST

putRequest :: ∀ a m r . WriteForeign a => MonadAsk (Env r) m => MonadAff m => String -> a -> m (Request String)
putRequest = writeRequest PUT

formatErr :: ∀ m a. MonadThrow Error m => ResponseFormatError -> m a
formatErr = throwError <<< error <<< printResponseFormatError -- >>> spy "Response Format Error"

jsonErr :: ∀ m f a.  MonadThrow Error m => Foldable f => f ForeignError -> m a
jsonErr = throwError <<< error <<< foldMap renderForeignError -- >>> spy "Foreign error"

deserialiseProblem :: ∀ m a b. MonadThrow Error m => ReadForeign a => Either ResponseFormatError String -> m (Either a b)
deserialiseProblem = either formatErr (readJSON >>> either jsonErr (pure <<< Left))

readJson :: ∀ m f a. MonadThrow Error m => ReadForeign a => Applicative f => Either ResponseFormatError String -> m (f a)
readJson = either formatErr (readJSON >>> either jsonErr (pure <<< pure))

deserialise_ :: ∀ m . MonadThrow Error m => Response (Either ResponseFormatError String) -> m (Either Problem Unit)
deserialise_ { body, status: StatusCode code} =
    if code # between 200 299
      then pure <<< pure $ unit
      else deserialiseProblem body -- # spy "Broken Response"

deserialise :: ∀ m a . MonadThrow Error m => ReadForeign a => Response (Either ResponseFormatError String) -> m (Either Problem a)
deserialise { body, status: StatusCode code} =
    if code # between 200 299
      then readJson body -- # spy "Happy Response" body
      else deserialiseProblem body # spy "Broken Response"

unhandled :: ∀ a m. MonadThrow Error m => Problem -> m a
unhandled p = throwError <<< error $ "Unhandled response code " <> writeJSON p

catchErrors
  :: ∀ a b m m'
   . Applicative m
  => Applicative m'
  => (a -> m (m' b))
  -> Either a b
  -> m (m' b)
catchErrors f = case _ of
  Left p -> f p
  Right r -> pure <<< pure $ r

getEventTypes
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => m (NakadiResponse () (Array EventType))
getEventTypes = do
  res <- getRequest "/event-types" >>= request >>= deserialise
  res # catchErrors case _ of
    p @ { status: 401 } -> pure $ lmap e401 res
    p -> unhandled p

postEventType
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => EventType
  -> m (NakadiResponse (conflict :: E409, unprocessableEntity :: E422) Unit)
postEventType eventType = do
  res <- postRequest "/event-types" eventType >>= request >>= deserialise_
  res # catchErrors case _ of
    p @ { status: 401 } -> pure $ lmap e401 res
    p @ { status: 409 } -> pure $ lmap e409 res
    p @ { status: 422 } -> pure $ lmap e422 res
    p -> unhandled p

getEventType
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => EventTypeName
  -> m (NakadiResponse (notFound :: E404) EventType)
getEventType (EventTypeName name) = do
  res <- getRequest ("/event-types/" <> name) >>= request >>= deserialise
  res # catchErrors case _ of
    p @ { status: 401 } -> pure $ lmap e401 res
    p @ { status: 404 } -> pure $ lmap e404 res
    p -> unhandled p

putEventType
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => EventTypeName
  -> EventType
  -> m (NakadiResponse (forbidden :: E403, notFound :: E404, unprocessableEntity :: E422) Unit)
putEventType (EventTypeName name) eventType = do
  res <- putRequest ("/event-types/" <> name) eventType >>= request >>= deserialise_
  res # catchErrors case _ of
    p @ { status: 401 } -> pure $ lmap e401 res
    p @ { status: 403 } -> pure $ lmap e403 res
    p @ { status: 404 } -> pure $ lmap e404 res
    p @ { status: 422 } -> pure $ lmap e422 res
    p -> unhandled p

deleteEventType
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => EventTypeName
  -> m (NakadiResponse (forbidden :: E403, notFound :: E404) Unit)
deleteEventType (EventTypeName name) = do
  res <- deleteRequest ("/event-types/" <> name) >>= request  >>= deserialise_
  res # catchErrors case _ of
    p @ { status: 401 } -> pure $ lmap e401 res
    p @ { status: 403 } -> pure $ lmap e403 res
    p @ { status: 404 } -> pure $ lmap e404 res
    p -> unhandled p

getCursorDistances
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => EventTypeName
  -> (Array CursorDistanceQuery)
  -> m (NakadiResponse (forbidden :: E403, notFound :: E404, unprocessableEntity :: E422) (Array CursorDistanceResult))
getCursorDistances (EventTypeName name) queries = do
  let path = "/event-types/" <> name <> "/cursor-distances"
  res <- postRequest path queries >>= request >>= deserialise
  res # catchErrors case _ of
    p @ { status: 401 } -> pure $ lmap e401 res
    p @ { status: 403 } -> pure $ lmap e403 res
    p @ { status: 404 } -> pure $ lmap e404 res
    p -> unhandled p

getCursorLag
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => EventTypeName
  -> (Array Cursor)
  -> m (NakadiResponse (forbidden :: E403, notFound :: E404, unprocessableEntity :: E422) (Array Partition))
getCursorLag (EventTypeName name) cursors = do
  let path = "/event-types/" <> name <> "/cursors-lag"
  res <- postRequest path cursors >>= request >>= deserialise
  res # catchErrors case _ of
    p @ { status: 401 } -> pure $ lmap e401 res
    p @ { status: 403 } -> pure $ lmap e403 res
    p @ { status: 404 } -> pure $ lmap e404 res
    p -> unhandled p

postEvents
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => EventTypeName
  -> (Array Event)
  -> m (NakadiResponse (multiStatus :: E207, forbidden :: E403, notFound :: E404, unprocessableEntity :: E422) Unit)
postEvents (EventTypeName name) events = do
  let path = "/event-types/" <> name <> "/events"
  { body, status: StatusCode statusCode } <- postRequest path events >>= request
  res1 <- case statusCode of
    207 -> map Just <$> readJson body
    code | code # between 200 299 -> (pure <<< pure) Nothing
    _ -> deserialiseProblem body
  res2 <- res1 # catchErrors case _ of -- is this all correct?
    p @ { status: 403 } -> pure $ lmap e403 res1
    p @ { status: 404 } -> pure $ lmap e404 res1
    p @ { status: 422 } -> pure $ lmap e422 res1
    p -> unhandled p
  pure $ res2 >>= case _ of
    Just multiStatus -> Left $ e207 multiStatus -- this is treated as an error
    Nothing -> Right unit

postSubscription
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => Subscription
  -> m (NakadiResponse (badRequest :: E400, unprocessableEntity :: E422) Subscription)
postSubscription subscription = do
  let path = "/subscriptions"
  res <- postRequest path subscription >>= request >>= deserialise
  res # catchErrors case _ of
    p @ { status: 400 } -> pure $ lmap e400 res
    p @ { status: 401 } -> pure $ lmap e401 res
    p @ { status: 422 } -> pure $ lmap e422 res
    p -> unhandled p

commitCursors
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => SubscriptionId
  -> XNakadiStreamId
  -> Array SubscriptionCursor
  -> m (NakadiResponse (forbidden :: E403, notFound :: E404, unprocessableEntity :: E422) Unit)
commitCursors (SubscriptionId id) (XNakadiStreamId header) items = do
  let path = "/subscriptions/" <> id <> "/cursors"
  standardRequest <- postRequest path { items }
  let req = standardRequest { headers = standardRequest.headers <> [RequestHeader "X-Nakadi-StreamId" header] }
  res <- request req >>= deserialise_
  res # catchErrors case _ of
    p @ { status: 401 } -> pure $ lmap e401 res
    p @ { status: 403 } -> pure $ lmap e403 res
    p @ { status: 422 } -> pure $ lmap e422 res
    p -> unhandled p

streamSubscriptionEvents
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => SubscriptionId
  -> StreamParameters
  -> (Array Event -> Aff Unit)
  -> m Unit -- [TODO] handle errors
streamSubscriptionEvents sid@(SubscriptionId subId) params handler = do
  env <- ask
  headers' <- baseHeaders
  let additionalHeaders =
        [ Tuple "Content-Type"    "application/json"
        , Tuple "Accept"          "application/json"
        , Tuple "Accept-Encoding" "gzip"
        ]
  let headers = Object.fromFoldable $ headers' <> additionalHeaders
  let https = String.stripPrefix (String.Pattern "https://") env.baseUrl
  let http  = String.stripPrefix (String.Pattern "http://") env.baseUrl
  let hostname = fromMaybe env.baseUrl (https <|> http)
  let protocol = if isJust http then "http:" else "https:"
  let options = HTTP.protocol := protocol
             <> HTTP.hostname := hostname
             <> HTTP.port     := env.port
             <> HTTP.headers  := HTTP.RequestHeaders headers
             <> HTTP.method   := "POST"
             <> HTTP.path     := ("/subscriptions/" <> subId <> "/events")


  _ <- liftAff <<< makeAff $ (\callback -> do
              req <- HTTP.request options (handle callback env)
              let writable = HTTP.requestAsStream req
              let endStream = Console.log "closing..." *> Stream.end writable (Console.log "closed")
              failed <- Stream.writeString writable UTF8 (writeJSON params) endStream
              when failed (callback (Left (error "Writing to outgoing stream failed.")))
              pure $ effectCanceler endStream
            )
  pure unit
  where
    -- [todo] way too big
    handle callback env response = do
      let stream = HTTP.responseAsStream response

      -- Error out if there's an unhappy error code
      if (HTTP.statusCode response >= 400)
        then (callback $ Left (error ("Could not stream " <> show (HTTP.statusCode response))))
        else do
          -- Should be alright let's process events
          xStreamId <- getStreamHeader response
          let chunk = String.split (String.Pattern "\n")

          let handleError = case _ of
                Left e -> do
                  liftEffect $ callback (Left <<< error $ "Error in processing " <> show e)
                Right a -> pure unit

          let commit cursors = if cursors == [] then pure (pure unit) else commitCursors sid (XNakadiStreamId xStreamId) cursors

          unzip <- Gzip.gunzip
          _ <- pipe stream unzip
          buffer <- liftEffect $ Ref.new ""

          -- [todo] make it readable
          onDataString unzip UTF8 (\resp -> runAff_ handleError $ do
            prev <- liftEffect $ Ref.read buffer
            let chunked = chunk (prev <> resp)
            let processable =  Array.dropEnd 1 chunked
            _ <- liftEffect $ Ref.modify_ (const (fromMaybe "" (Array.last chunked))) buffer
            let decoded = traverse readJSON processable
            batch :: Array SubscriptionEventStreamBatch <- either jsonErr pure decoded
            let events = batch <#> _.events <#> fromMaybe []
            results <- traverse handler events
            let cursors = batch <#> _.cursor
            commitResult <- commit cursors # flip runReaderT env
            liftEffect $ case commitResult of
              Left err -> callback <<< Left <<< error $ show err
              Right _ -> pure unit
            pure unit
            )

getStreamHeader :: HTTP.Response -> Effect String
getStreamHeader response = do
  let responseHeaders = HTTP.responseHeaders response
  -- [WARN]: response headers are always lowercased
  let headerName = "x-nakadi-streamid"
  Object.lookup headerName responseHeaders
    # maybe (throwException (error $ "Missing " <> headerName <> " header")) pure