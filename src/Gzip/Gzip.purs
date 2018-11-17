module Gzip.Gzip where

import Effect (Effect)
import Node.Stream (Duplex)

foreign import gunzip :: Effect Duplex
