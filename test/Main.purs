module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, run')

main :: Effect Unit
main = do
  let conf = defaultConfig { timeout = Just 600000}
  discover "Test\\..*Spec" >>= run' conf [consoleReporter]
