module Node.Stream.Util where

import Prelude

import Data.Options (Option, opt)
import Effect (Effect)
import Foreign (Foreign)
import Node.Buffer (Buffer)
import Node.HTTP.Client (RequestOptions)

foreign import splitAtNewlineImpl ::
  (Foreign -> Effect Unit) -> Effect (Buffer -> Effect Unit)

foreign import data Agent :: Type

foreign import newHttpsKeepAliveAgent :: Effect Agent
foreign import newHttpKeepAliveAgent :: Effect Agent

-- | The agent to use
agent :: Option RequestOptions Agent
agent =  opt "agent"
