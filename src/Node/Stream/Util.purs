module Node.Stream.Util where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Options (Option, opt)
import Effect (Effect)
import Foreign (Foreign)
import Node.Buffer (Buffer)
import Node.HTTP.Client (RequestOptions)

foreign import splitAtNewlineImpl ∷
  Fn3 Buffer Int (Foreign -> Effect Unit) (Effect (Buffer -> Effect Unit))

foreign import allocUnsafeImpl ∷ Int -> Effect Buffer

allocUnsafe ∷ BufferSize -> Effect Buffer
allocUnsafe (BufferSize bs) = allocUnsafeImpl bs

newtype BufferSize = BufferSize Int

splitAtNewline ∷ Buffer -> BufferSize -> (Foreign -> Effect Unit) -> Effect (Buffer -> Effect Unit)
splitAtNewline b (BufferSize bs) = runFn3 splitAtNewlineImpl b bs


foreign import data Agent ∷ Type

foreign import newHttpsKeepAliveAgent ∷ Effect Agent
foreign import newHttpKeepAliveAgent ∷ Effect Agent

-- | The agent to use
agent ∷ Option RequestOptions Agent
agent =  opt "agent"
