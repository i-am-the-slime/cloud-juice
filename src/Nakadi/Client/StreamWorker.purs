module Nakadi.Client.StreamWorker where

import Prelude

import Effect (Effect)
import Foreign.Object (Object)

foreign import workerImpl ∷ ∀ a b. Object (a -> b) -> Effect Unit
