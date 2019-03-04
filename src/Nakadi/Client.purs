module Nakadi.Client
 ( getEventTypes
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

import Affjax.RequestHeader (RequestHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (over, unwrap)
import Data.Options ((:=))
import Data.String as String
import Data.Time.Duration (Milliseconds(..), Seconds(..), convertDuration)
import Data.Tuple (Tuple(..))
import Data.Variant (default, on)
import Effect (Effect)
import Effect.Aff (Aff, delay, message)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Retry (RetryPolicyM, RetryStatus, constantDelay, limitRetries, recovering)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Effect.Ref as Ref
import Foreign.Object as Object
import Nakadi.Client.Internal (catchErrors, deleteRequest, deserialise, deserialiseProblem, deserialise_, getRequest, postRequest, putRequest, readJson, request, unhandled)
import Nakadi.Client.Stream (CommitResult, StreamReturn(..), postStream)
import Nakadi.Client.Types (Env, NakadiResponse)
import Nakadi.Errors (E207, E400, E403, E404, E409(..), E422(..), E422Publish, _conflict, _unprocessableEntity, e207, e400, e401, e403, e404, e409, e422, e422Publish)
import Nakadi.Types (Cursor, CursorDistanceQuery, CursorDistanceResult, Event, EventType, EventTypeName(..), Partition, StreamParameters, Subscription, SubscriptionCursor, SubscriptionId(..), XNakadiStreamId(..))
import Node.Encoding (Encoding(..))
import Node.HTTP.Client (Request)
import Node.HTTP.Client as HTTP
import Node.Stream as Stream
import Simple.JSON (writeJSON)


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
  -> m (NakadiResponse (multiStatus :: E207, forbidden :: E403, notFound :: E404, unprocessableEntityPublish :: E422Publish) Unit)
postEvents (EventTypeName name) events = do
  let path = "/event-types/" <> name <> "/events"
  { body, status: StatusCode statusCode } <- postRequest path events >>= request
  res1 <- case statusCode of
    code | code == 422 || code == 207 -> map Just <$> readJson body
    code | code # between 200 299 -> (pure <<< pure) Nothing
    _ -> deserialiseProblem body
  res2 <- res1 # catchErrors case _ of -- is this all correct?
    p @ { status: 403 } -> pure $ lmap e403 res1
    p @ { status: 404 } -> pure $ lmap e404 res1
    p -> unhandled p
  pure $ res2 >>= case _ of
    Just batchProblem ->
      if statusCode == 207
      then Left $ e207 batchProblem -- this is treated as an error
      else Left $ e422Publish batchProblem
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
  -> m CommitResult
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


foreign import removeRequestTimeout :: Request -> Effect Unit

streamSubscriptionEvents
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadError Error m
  => MonadAff m
  => SubscriptionId
  -> StreamParameters
  -> (Array Event -> Aff Unit)
  -> m StreamReturn
streamSubscriptionEvents sid@(SubscriptionId subId) streamParameters eventHandler = do
  env   <- ask

  let listen postArgs = do
        token <- env.token
        let headers' =
              [ Tuple "X-Flow-ID" (unwrap env.flowId)
              , Tuple "Authorization" token
              , Tuple "Content-Type"    "application/json"
              , Tuple "Accept"          "application/json"
              -- , Tuple "Accept-Encoding" "gzip"
              ]
        let headers = Object.fromFoldable headers'
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

        let requestCallback = postStream postArgs streamParameters commitCursors sid eventHandler env
        req <- HTTP.request options requestCallback
        removeRequestTimeout req
        let writable = HTTP.requestAsStream req
        let body = writeJSON streamParameters
        Stream.onClose writable (Console.log "request written!")
        Stream.onEnd writable (Console.log "request end!!!")
        Stream.onFinish writable (Console.log "request finish!!!")
        Stream.onError writable (\e -> Console.log $ "Error!!!" <> message e)
        let endStream = do
              Console.log "Destroying stream"
              Stream.end writable (pure unit)
        _ <- Stream.writeString writable UTF8 body endStream
        pure unit

  let baseBackOff = 1.0 # Seconds
  backOffRef <- liftEffect $ Ref.new baseBackOff
  let resetBackOff = liftEffect $ do
        Ref.write baseBackOff backOffRef

  let
    go :: m StreamReturn
    go = do
        resultVar <- liftAff AVar.empty
        batchesVar <- liftAff AVar.empty
        liftAff $ AVar.put [] batchesVar
        let postArgs = { resultVar
                       , batchesVar
                       , onStreamEstablished: resetBackOff
                       }
        destroyStream <- liftEffect $ listen postArgs
        res <- liftAff $ AVar.take resultVar

        let retry prob errMsg = do
              bo@(Seconds backOff) <- liftEffect $ Ref.read backOffRef
              let fullMessage = errMsg <> " Cancelling stream and retrying in "
                                       <> show (round backOff) <> " seconds"
              liftEffect $ env.logWarn prob fullMessage
              liftAff (delay (convertDuration bo))
              liftEffect $ Ref.modify_ (over Seconds (2.0 * _)) backOffRef
              go

        -- Handle errors
        case res of
          StreamClosed ->
            retry Nothing "Stream closed by Nakadi"
          FailedToStream err -> err #
            on _conflict (\(E409 p) -> retry (Just p) "Failed to start streaming.")
            (default (pure res))

          FailedToCommit err -> err #
            on _unprocessableEntity (\(E422 p) -> retry (Just p) "Failed to commit cursor.")
            (default (pure res))

  let
    retryPolicy :: RetryPolicyM m
    retryPolicy = constantDelay (200.0 # Milliseconds) <> limitRetries 10
  let
    retryChecks :: Array (RetryStatus -> Error -> m Boolean)
    retryChecks = [\_ _ -> pure true]

  recovering retryPolicy retryChecks (const go)
