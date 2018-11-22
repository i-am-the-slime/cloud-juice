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
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Options ((:=))
import Data.String as String
import Data.Time.Duration (Seconds(..), convertDuration)
import Data.Tuple (Tuple(..))
import Data.Variant (default, on)
import Effect.Aff (Aff, delay, effectCanceler, forkAff, makeAff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import Effect.Exception (Error, error, throwException)
import Foreign.Object as Object
import Nakadi.Client.Internal (baseHeaders, catchErrors, deleteRequest, deserialise, deserialiseProblem, deserialise_, getRequest, postRequest, putRequest, readJson, request, unhandled)
import Nakadi.Client.Stream (StreamReturn, postStream, CommitResult)
import Nakadi.Client.Types (Env, NakadiResponse)
import Nakadi.Errors (E207, E400, E403, E404, E409, E422, _conflict, e207, e400, e401, e403, e404, e409, e422)
import Nakadi.Types (Cursor, CursorDistanceQuery, CursorDistanceResult, Event, EventType, EventTypeName(..), Partition, StreamParameters, Subscription, SubscriptionCursor, SubscriptionId(..), XNakadiStreamId(..))
import Node.Encoding (Encoding(..))
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


streamSubscriptionEvents
  :: ∀ r m
   . MonadAsk (Env r) m
  => MonadThrow Error m
  => MonadAff m
  => SubscriptionId
  -> StreamParameters
  -> (Array Event -> Aff Unit)
  -> m StreamReturn
streamSubscriptionEvents sid@(SubscriptionId subId) streamParameters eventHandler = do
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


  let listen resultVar _ = do
        req <- HTTP.request options (postStream resultVar commitCursors sid eventHandler env)
        let writable = HTTP.requestAsStream req
        let body = writeJSON streamParameters
        let endStream = Stream.end writable $ pure unit
        failed <- Stream.writeString writable UTF8 body endStream
        when failed $ (throwException <<< error) "Writing to outgoing stream failed."
        pure $ effectCanceler endStream

  let baseBackOff = 1.0 # Seconds

  let go bo@(Seconds backOff) = do
        rv <- liftAff AVar.empty
        _ <- liftAff <<< forkAff <<< makeAff $ listen rv
        res <- liftAff $ AVar.take rv
        case res of
          Right canceler -> pure (Right canceler)
          Left err -> (
            default (pure res)
              # (on _conflict (\_ -> do
                  Console.error
                    $ "No flee slots available. Retrying in "
                    <> show (round backOff)
                    <> " seconds"
                  liftAff (delay (convertDuration bo))
                  go (2.0 * backOff # Seconds))
                )
            ) (err)
  go baseBackOff
