module Test.ApiSpec where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..), hush)
import Data.List.Lazy (toUnfoldable)
import Data.List.Lazy as List
import Data.List.Lazy.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Time.Duration (class Duration, Milliseconds(..))
import Data.Traversable (sequence_, traverse_)
import Data.Variant (expand)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (delay, error, forkAff, killFiber, supervise)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FlowId.Types (FlowId(..))
import Nakadi.Client (Env, deleteEventType, getEventType, getEventTypes, postEventType, postEvents, postSubscription, putEventType, streamSubscriptionEvents)
import Nakadi.Minimal as Minimal
import Nakadi.Types (Event(..), EventTypeName(..), OwningApplication(..), SubscriptionId(..))
import Node.Encoding (Encoding(..))
import Node.Process (stdout)
import Node.Stream (writeString)
import Simple.JSON (class WriteForeign, writeImpl, writeJSON)
import Test.Spec (Spec, describe, it, itOnly, pending, pending')
import Test.Spec.Assertions (shouldEqual)


env :: Env ()
env =
  { flowId: FlowId "assbox"
  , baseUrl: "http://localhost"
  -- , baseUrl: "https://nakadi-staging.aruha-test.zalan.do"
  , token: pure "eyJraWQiOiJwbGF0Zm9ybS1pYW0tdmNlaHloajYiLCJhbGciOiJFUzI1NiJ9.eyJzdWIiOiJhM2U2MzdlMS01YTk5LTRmYzYtOGY5OS0xMzJkYmIzMzFkYjQiLCJodHRwczovL2lkZW50aXR5LnphbGFuZG8uY29tL3JlYWxtIjoidXNlcnMiLCJodHRwczovL2lkZW50aXR5LnphbGFuZG8uY29tL3Rva2VuIjoiQmVhcmVyIiwiaHR0cHM6Ly9pZGVudGl0eS56YWxhbmRvLmNvbS9tYW5hZ2VkLWlkIjoibWVpYmVzIiwiYXpwIjoienRva2VuIiwiaHR0cHM6Ly9pZGVudGl0eS56YWxhbmRvLmNvbS9icCI6IjgxMGQxZDAwLTQzMTItNDNlNS1iZDMxLWQ4MzczZmRkMjRjNyIsImF1dGhfdGltZSI6MTU0MjA1MDQ3MCwiaXNzIjoiaHR0cHM6Ly9pZGVudGl0eS56YWxhbmRvLmNvbSIsImV4cCI6MTU0MjIxMjg3OSwiaWF0IjoxNTQyMjA5MjY5fQ.4rpTe6MzivZRMtZcTR2JrXoyu3jXyUhhcPUvxfhfp13ZP044f4BSw6C2DqGUf40EhocIjvbveGHl8mdPuh-5Yw"
  , port: 8080
  -- , port: 443
  }

run :: ∀ m a. ReaderT (Env ()) m a -> m a
run = flip runReaderT env

spec :: Spec Unit
spec =
  describe "api" $ do
    let undefinedET = Minimal.eventType
          (EventTypeName "test-undefined")
          (OwningApplication "cloud-juice")
          (Minimal.eventTypeSchema)
    let undefinedETN = undefinedET # _.name

    -- let dataChangeET =
    --   (Minimal.eventType
    --     (EventTypeName "test-data-change") "cloud-juice")
    --     (Minimal.schema {}))
    --     { category =  "data"
    --     }

    describe "/event-types" $ do
      -- itOnly "Test" $ do
      --   testFile <- liftEffect $ readTextFile UTF8 "./test/event-types-staging.json"
      --   let r = (lmap (spy "Boy")) (readJSON testFile)
      --   let (result :: Maybe (Array EventType)) = hush r
      --   (r <#> (map getSchema)) `shouldEqual` (Right [])
      it "Preparation" $ do
        res <- run <<< runExceptT $ do
          types <- ExceptT $ (map <<< lmap $ expand) getEventTypes
          traverse_ (ExceptT <<< deleteEventType) (types <#> _.name)
        res `shouldEqual` (Right unit)
      it "POST" $ do
        res <- run $ postEventType undefinedET
        res `shouldEqual` (Right unit)
      it "GET" $ do
        res <- run getEventTypes
        (res <#> (map _.name)) `shouldEqual` (Right [undefinedETN])

    describe "/event-types/{name}" $ do
      it "GET" $ do
        et <- run $ getEventType undefinedETN
        (et <#> _.name) `shouldEqual` (Right undefinedETN)
      it "PUT" $ do
        let modified = undefinedET { owning_application = OwningApplication "other-app" }
        put <- run $ putEventType undefinedETN modified
        put `shouldEqual` (Right unit)
        get <- run $ getEventType undefinedETN
        (get <#> _.owning_application) `shouldEqual` (Right modified.owning_application)
      it "DELETE" $ do
        res <- run $ deleteEventType undefinedETN
        res `shouldEqual` (Right unit)

    describe "/event-types/{name}/cursor-distances" $ do
      pending "POST"
    describe "/event-types/{name}/cursors-lag" $ do
      pending "POST"
    describe "/event-types/{name}/events" $ do
      -- pending "GET" -- deprecated, let's not support it
      it "POST" $ do
        let events = bigEvent #
                      writeImpl >>> UndefinedEvent >>>
                      NEL.repeat >>> NEL.tail >>>
                      List.take 100 >>> toUnfoldable
        -- make sure it exists
        _ <- run $ postEventType undefinedET
        res <- sequence_ (List.take 100 $ List.repeat (
            (delay (200.0 # Milliseconds)) *>
            (run $ postEvents undefinedETN events)
          ))
        res `shouldEqual` unit
    describe "/event-types/{name}/partitions" $ do
      pending "GET"
    describe "/event-types/{name}/partitions/{partition}" $ do
      pending "GET"
    describe "/event-types/{name}/schemas" $ do
      pending "GET"
    describe "/event-types/{name}/schemas/{version}" $ do
      pending "GET"
    describe "/event-types/{name}/shifted-cursors" $ do
      pending "POST"
    describe "/event-types/{name}/timelines" $ do
      pending "GET"
      pending "POST"
    describe "/metrics" $ do
      pending "GET"
    describe "/registry/enrichment-strategies" $ do
      pending "GET"
    describe "/registry/partition-strategies" $ do
      pending "GET"
    describe "/settings/admins" $ do
      pending "GET"
      pending "POST"
    describe "/settings/blacklist" $ do
      pending "GET"
    describe "/settings/blacklist/{blacklist_type}/{name}" $ do
      pending "PUT"
      pending "DELETE"
    describe "/settings/features" $ do
      pending "GET"
      pending "POST"
    describe "/storages" $ do
      pending "GET"
      pending "POST"
    describe "/storages/default/{id}" $ do
      pending "PUT"
    describe "/storages/{id}" $ do
      pending "GET"
      pending "DELETE"
    describe "/subscriptions" $ do
      pending "GET"
      pending "POST"
    describe "/subscriptions/{subscription_id}" $ do
      pending "GET"
      pending "PUT"
      pending "DELETE"
    describe "/subscriptions/{subscription_id}/cursors" $ do
      pending "GET"
      pending "POST"
      pending "PATCH"
    describe "/subscriptions/{subscription_id}/events" $ do
      pending "GET"
      it "POST (local)" $ do
        -- let events = { "korkenzieher": "milchhof brodowin" } #
        --               writeImpl >>> UndefinedEvent >>>
        --               NEL.repeat >>> NEL.tail >>>
        --               List.take 5000 >>> toUnfoldable
        -- _ <- run $ postEventType undefinedET
        -- _ <- replicateM 5000 $ run $ postEvents undefinedETN events
        let subs = Minimal.subscription (OwningApplication "cloud-juice") undefinedETN
        sub <- run $ postSubscription subs
        (void sub) `shouldEqual` (Right unit)
        let subId = fromMaybe (SubscriptionId "nein") (hush sub >>= _.id)
        events <- liftEffect $ Ref.new 0
        characters <- liftEffect $ Ref.new 0
        startTime <- liftEffect now
        run $ streamSubscriptionEvents subId (Minimal.streamParameters 20) (handler events characters startTime)
        delay (Milliseconds 30000.0)
        pure unit
      pending' "POST (staging)" $ do
        let etn = EventTypeName "availability-layer.retail-product_00f2a393-6889-4fc0-8cd9-86e454e6dfa3"
        -- let subs = Minimal.subscription (OwningApplication "cloud-juice") etn
        -- sub <- run $ postSubscription subs
        -- (void sub) `shouldEqual` (Right unit)
        -- let subId = fromMaybe (SubscriptionId "nein") (hush sub >>= _.id)
        let subId = SubscriptionId "7bef3ddc-5a6a-4ac7-b904-2f0cc71efcd6"
        events <- liftEffect $ Ref.new 0
        characters <- liftEffect $ Ref.new 0
        startTime <- liftEffect now
        fiber <- forkAff (supervise (
          run $ streamSubscriptionEvents subId (Minimal.streamParameters 1000) (handler events characters startTime)
        ))
        delay (10000.0 # Milliseconds)
        Console.log "\nTime's up!"
        killFiber (error "Timeout") fiber
        pure unit

    describe "/subscriptions/{subscription_id}/stats" $ do
      pending "GET"

handler :: ∀ a. WriteForeign a => Ref Int -> Ref Int -> Instant -> Array a -> Effect Unit
handler events characters startTime x = liftEffect $ do
  countBefore <- Ref.read events
  let asJson = writeJSON x
  Ref.modify_ (_ + String.length asJson) characters
  Ref.modify_ (_ + Array.length x) events
  count <- Ref.read events
  chars <- Ref.read characters
  let megabytes = chars / (1024*1024)
  -- when ((countBefore `div` 20) < (count `div` 20))
  void (writeString stdout UTF8 ("\r" <> show count <> " events processed (" <> show megabytes <> " MB)") (pure unit))
  if count /= 10000 then  pure unit else (now >>= (\endTime -> Console.log ("\n" <> show ((unwrap (unInstant endTime)) - (unwrap (unInstant startTime))))))


bigEvent :: { korkenzieher :: String , items :: Array { korkenzieher :: Array String } }
bigEvent =
  { "korkenzieher": "milchhof brodowin"
  , "items":
    [ { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    , { "korkenzieher": ["milchhof", "brodowin"]}
    ]
  }