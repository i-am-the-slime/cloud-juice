module Nakadi.Client.Types
  ( Env
  , NakadiResponse
  )
  where

import Data.Either (Either)
import Data.Variant (Variant)
import Effect (Effect)
import FlowId.Types (FlowId)
import Nakadi.Errors (E401)

type Env r =
  { flowId  :: FlowId
  , baseUrl :: String
  , port    :: Int
  , token   :: Effect String
  | r
  }

type NakadiResponse r a = Either (Variant (unauthorized :: E401 | r)) a