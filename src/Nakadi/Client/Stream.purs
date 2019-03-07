module Nakadi.Client.Stream
  ( postStream
  , StreamReturn(..)
  , StreamError
  , CommitError
  , CommitResult
  )
  where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_, message, runAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import Foreign (Foreign)
import Foreign.Object as Object
import Gzip.Gzip as Gzip
import Nakadi.Client.Internal (catchErrors, jsonErr, unhandled)
import Nakadi.Client.Types (NakadiResponse, Env)
import Nakadi.Errors (E400, E401, E403, E404, E409, E422, e400, e401, e403, e404, e409)
import Nakadi.Types (Event, StreamParameters, SubscriptionCursor, SubscriptionId, XNakadiStreamId(..), SubscriptionEventStreamBatch)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(..))
import Node.HTTP.Client as HTTP
import Node.Stream (Read, Stream, onData, onDataString, onEnd, onError, pipe)
import Node.Stream as Stream
import Node.Stream.Util (BufferSize, splitAtNewline)
import Simple.JSON (E, readJSON)
import Simple.JSON as JSON

type StreamError = Variant
  ( unauthorized ∷ E401
  , badRequest ∷ E400
  , forbidden  ∷ E403
  , notFound   ∷ E404
  , conflict   ∷ E409
  )

type CommitError = Variant
  ( unauthorized ∷ E401
  , forbidden ∷ E403
  , notFound ∷ E404
  , unprocessableEntity ∷ E422
  )

data StreamReturn
  = FailedToStream StreamError
  | FailedToCommit CommitError
  | StreamClosed

type EventHandler = Array Event -> Aff Unit

type CommitResult =
  NakadiResponse (forbidden ∷ E403, notFound ∷ E404, unprocessableEntity ∷ E422) Unit

postStream
  ∷ ∀ r
  . { resultVar ∷ AVar StreamReturn
    , buffer ∷ Buffer
    , bufsize ∷ BufferSize
    }
  -> StreamParameters
  -> (SubscriptionId -> XNakadiStreamId -> Array SubscriptionCursor -> ReaderT (Env r) Aff CommitResult)
  -> SubscriptionId
  -> EventHandler
  -> Env r
  -> HTTP.Response
  -> Effect Unit
postStream { resultVar, buffer, bufsize } streamParams commitCursors subscriptionId eventHandler env response = do
  let _ = HTTP.statusMessage response
  let isGzipped = getHeader "Content-Encoding" response <#> String.contains (String.Pattern "gzip") # fromMaybe false
  let baseStream = HTTP.responseAsStream response

  Stream.onClose baseStream (launchAff_ $ AVar.put StreamClosed resultVar)
  stream <-
    if isGzipped
    then do
      gunzip <- Gzip.gunzip
      Stream.onError gunzip (const $ pure unit)
      pipe baseStream gunzip
    else
      pure baseStream

  if HTTP.statusCode response /= 200
    then handleRequestErrors resultVar stream
    else do
      -- Positive response, so we reset the backoff
      xStreamId <- XNakadiStreamId <$> getHeaderOrThrow "X-Nakadi-StreamId" response
      let commit = mkCommit xStreamId
      handleRequest { resultVar, buffer, bufsize } streamParams stream commit eventHandler env
  where
    mkCommit xStreamId cursors =
      if cursors == mempty
      then do pure (Right unit)
      else commitCursors subscriptionId xStreamId cursors

handleUnhandledError ∷ Either Error Unit -> Effect Unit
handleUnhandledError = case _ of
    Left e -> do
      liftEffect $ throwException (error $ "Error in processing " <> show e)
    Right a -> pure unit

handlePutAVarError ∷ Either Error Unit -> Effect Unit
handlePutAVarError = case _ of
    Left e -> do
      liftEffect $ throwException (error $ "Error setting AVar " <> show e)
    Right a -> pure unit

handleRequest
  ∷ ∀ env r
  . { resultVar ∷ AVar StreamReturn
    , buffer ∷ Buffer
    , bufsize ∷ BufferSize
    }
  -> StreamParameters
  -> Stream (read ∷ Read | r)
  -> (Array SubscriptionCursor -> ReaderT (Env env) Aff CommitResult)
  -> EventHandler
  -> Env env
  -> Effect Unit
handleRequest { resultVar, buffer, bufsize } streamParams resStream commit eventHandler env = do
  callback <- splitAtNewline buffer bufsize  handle
  onData  resStream callback
  onEnd   resStream (launchAff_ $ AVar.put StreamClosed resultVar)
  onError resStream (\e -> do
    env.logWarn Nothing $ "Error in read stream " <> message e
    launchAff_ $ AVar.put StreamClosed resultVar
    )
  where
    handle eventStreamBatch = runAff_ handleUnhandledError $ do
        let parseFn = JSON.read ∷ Foreign -> E SubscriptionEventStreamBatch
        batch <- either jsonErr pure (parseFn eventStreamBatch)
        traverse_ eventHandler batch.events
        commitResult <- runReaderT (commit [batch.cursor]) env
        case commitResult of
          Left err ->
            AVar.put (FailedToCommit err) resultVar
          Right other -> do
            pure unit

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
    case result of
      Left err -> AVar.put (FailedToStream err) resultVar
      Right _ -> pure unit
  where
  throwError = case _ of
    Left e -> do
      liftEffect $ throwException (error $ "Error in processing " <> show e)
    Right a -> pure unit

getHeader ∷ String -> HTTP.Response -> Maybe String
getHeader headerName response = do
  let responseHeaders = HTTP.responseHeaders response
  -- [WARN]: response headers are always lowercased
  let headerNameLower = String.toLower headerName
  Object.lookup headerNameLower responseHeaders

getHeaderOrThrow ∷ String -> HTTP.Response -> Effect String
getHeaderOrThrow headerName =
  maybe (throwException (error $ "Required header " <> headerName <> " is missing")) pure <<<
      getHeader headerName
