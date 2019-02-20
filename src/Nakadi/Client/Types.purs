module Nakadi.Client.Types
  ( Env
  , NakadiResponse
  )
  where

import Prelude
import Data.Either (Either)
import Data.Variant (Variant)
import Effect (Effect)
import FlowId (FlowId)
import Nakadi.Errors (E401)
import Nakadi.Types (Problem)

type Env r =
  { flowId  :: FlowId
  , baseUrl :: String
  , port    :: Int
  , token   :: Effect String
  , logWarn :: Problem -> String -> Effect Unit
  | r
  }

type NakadiResponse r a = Either (Variant (unauthorized :: E401 | r)) a
