module Nakadi.Client.Stream
  ( postStream
  , StreamReturn
  , CommitResult
  )
  where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import Foreign.Object as Object
import Gzip.Gzip as Gzip
import Nakadi.Client.Internal (catchErrors, jsonErr, unhandled)
import Nakadi.Client.Types (NakadiResponse, Env)
import Nakadi.Errors (E400, E403, E404, E409, E422, e400, e401, e403, e404, e409)
import Nakadi.Types (Event, SubscriptionCursor, SubscriptionEventStreamBatch, SubscriptionId, XNakadiStreamId(..))
import Node.Encoding (Encoding(..))
import Node.HTTP.Client as HTTP
import Node.Stream (Read, Stream, destroy, onDataString, onEnd, onReadable, pause, pipe, readString, resume)
import Simple.JSON (readJSON)

type StreamReturn
  = NakadiResponse
  ( badRequest :: E400
  , forbidden  :: E403
  , notFound   :: E404
  , conflict   :: E409
  ) (Effect Unit)

type EventHandler = Array Event -> Aff Unit

type CommitResult =
  NakadiResponse (forbidden :: E403, notFound :: E404, unprocessableEntity :: E422) Unit

postStream
  :: forall r
   . AVar StreamReturn
  -> (SubscriptionId -> XNakadiStreamId -> Array SubscriptionCursor -> ReaderT (Env r) Aff CommitResult)
  -> SubscriptionId
  -> EventHandler
  -> Env r
  -> HTTP.Response
  -> Effect Unit
postStream resultVar commitCursors subscriptionId eventHandler env response = do
  let _ = HTTP.statusMessage response
  let isGzipped = getHeader "Content-Encoding" response <#> String.contains (String.Pattern "gzip") # fromMaybe false
  let baseStream = HTTP.responseAsStream response
  stream <- if isGzipped then Gzip.gunzip >>= \gunzip -> pipe baseStream gunzip $> gunzip else pure baseStream

  if HTTP.statusCode response /= 200
    then handleRequestErrors resultVar stream
    else do
      runAff_ handlePutAVarError (AVar.put (Right $ destroy stream) resultVar)
      xStreamId <- XNakadiStreamId <$> getHeaderOrThrow "X-Nakadi-StreamId" response
      -- Positive response, so we reset the backoff
      let commit = mkCommit xStreamId
      handleRequest resultVar stream commit eventHandler env
  where
  mkCommit xStreamId cursors =
    if cursors == mempty
    then do pure (Right unit)
    else commitCursors subscriptionId xStreamId cursors

handleCommitError :: Either Error Unit -> Effect Unit
handleCommitError = case _ of
    Left e -> do
      liftEffect $ throwException (error $ "Error in processing " <> show e)
    Right a -> pure unit

handlePutAVarError :: Either Error Unit -> Effect Unit
handlePutAVarError = case _ of
    Left e -> do
      liftEffect $ throwException (error $ "Error setting AVar " <> show e)
    Right a -> pure unit

chunk :: String -> Array String
chunk = String.split $ String.Pattern "\n"

handleRequest
  :: forall env r
   . AVar StreamReturn
  -> Stream (read ∷ Read | r)
  -> (Array SubscriptionCursor -> ReaderT (Env env) Aff CommitResult)
  -> EventHandler
  -> Env env
  -> Effect Unit
handleRequest resultVar stream commit eventHandler env = do
  buffer <- liftEffect $ Ref.new ""
  -- onEnd stream (runAff_ handlePutAVarError (AVar.put (Right unit) resultVar))
  onReadable stream (handler buffer)
  where
    handler buffer = runAff_ handleCommitError $ do
      resp <- liftEffect $ fromMaybe "" <$> readString stream Nothing UTF8
      liftEffect $ pause stream
      -- Update buffer
      prev <- liftEffect $ Ref.read buffer
      let chunked = chunk (prev <> resp)
      let processable =  Array.dropEnd 1 chunked
      _ <- liftEffect $ Ref.modify_ (const (fromMaybe "" (Array.last chunked))) buffer

      -- Decode incoming data
      let decoded = traverse readJSON processable
      batch :: Array SubscriptionEventStreamBatch <- either jsonErr pure decoded
      let events = batch <#> _.events <#> fromMaybe []

      -- Handle events
      results <- traverse eventHandler events
      let (cursors :: Array SubscriptionCursor) = batch <#> _.cursor

      commitResult <- runReaderT (commit cursors) env
      liftEffect $ case commitResult of
        Left err -> throwException $ error (show err)
        Right _ -> pure unit
      liftEffect $ resume stream

handleRequestErrors ∷ ∀ r. AVar StreamReturn → Stream (read ∷ Read | r) -> Effect Unit
handleRequestErrors resultVar response = do
  buffer <- liftEffect $ Ref.new ""
  onDataString response UTF8 (\x -> Ref.modify_ (_ <> x) buffer)
  onEnd response $ runAff_ throwError do
    str <- liftEffect $ Ref.read buffer
    liftEffect <<< Console.log $ "Result: " <> str
    res <- (readJSON >>> either jsonErr (pure <<< Left)) str
    result <- res # catchErrors case _ of
      p @ { status: 400 } -> pure $ lmap e400 res
      p @ { status: 401 } -> pure $ lmap e401 res
      p @ { status: 403 } -> pure $ lmap e403 res
      p @ { status: 404 } -> pure $ lmap e404 res
      p @ { status: 409 } -> pure $ lmap e409 res
      p -> unhandled p
    AVar.put result resultVar
  where
  throwError = case _ of
    Left e -> do
      liftEffect $ throwException (error $ "Error in processing " <> show e)
    Right a -> pure unit

getHeader :: String -> HTTP.Response -> Maybe String
getHeader headerName response = do
  let responseHeaders = HTTP.responseHeaders response
  -- [WARN]: response headers are always lowercased
  let headerNameLower = String.toLower headerName
  Object.lookup headerNameLower responseHeaders

getHeaderOrThrow :: String -> HTTP.Response -> Effect String
getHeaderOrThrow headerName =
  maybe (throwException (error $ "Required header " <> headerName <> " is missing")) pure <<<
      getHeader headerName
