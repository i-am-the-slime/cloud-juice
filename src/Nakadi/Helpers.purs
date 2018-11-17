module Nakadi.Helpers where

import Data.Either (Either(..))
import Json.Schema (JsonSchema)
import Nakadi.Types (EventType, StringSchema(..))

getSchema :: EventType -> Either String JsonSchema
getSchema = case _ of
  { schema: { schema: ValidStringSchema jsonSchema }} -> Right jsonSchema
  { schema: { schema: BrokenStringSchema jsonSchema }} -> Left jsonSchema