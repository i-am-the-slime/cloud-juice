module Json.Schema where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign (F, Foreign, ForeignError(ForeignError), fail, readString, typeOf)
import Foreign.Index (readProp)
import Foreign.Object (Object)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype JsonSchema = JsonSchema
  { "type" :: Maybe TypeValidator
  , additionalProperties :: Maybe (Either Boolean JsonSchema)
  , description :: Maybe String
  , format :: Maybe String
  , items :: Maybe JsonSchema
  , oneOf :: Maybe (Array JsonSchema)
  , properties :: Maybe (Object JsonSchema)
  , ref :: Maybe SchemaRef
  , required :: Maybe (Array String)
  }

derive instance ntJsonSchema :: Newtype JsonSchema _
instance eqJsonSchema :: Eq JsonSchema where
  eq (JsonSchema s1) (JsonSchema s2) =
    all identity
      [ eq s1."type" s2."type"
      , eq s1.additionalProperties s2.additionalProperties
      , eq s1.description s2.description
      , eq s1.format s2.format
      , eq s1.items s2.items
      , eq s1.oneOf s2.oneOf
      , eq s1.properties s2.properties
      , eq s1.ref s2.ref
      , eq s1.required s2.required
      ]
-- derive newtype instance ordSchema :: Ord Schema
instance showSchema :: Show JsonSchema where
  show (JsonSchema s) = show s

instance readForeignSchema :: ReadForeign JsonSchema where
  readImpl f = do
    type_                <- readImpl =<< readProp "type" f
    additionalProperties <- addProps =<< readProp "additionalProperties" f
    description          <- readImpl =<< readProp "description" f
    format               <- readImpl =<< readProp "format" f
    items                <- readImpl =<< readProp "items" f
    oneOf                <- readImpl =<< readProp "oneOf" f
    properties           <- readImpl =<< readProp "properties" f
    ref                  <- readImpl =<< readProp "ref" f
    required             <- readImpl =<< readProp "required" f
    pure $ JsonSchema
      { "type": type_
      , additionalProperties
      , description
      , format
      , items
      , oneOf
      , properties
      , ref
      , required
      }
    where
      addProps (f' :: Foreign) = case typeOf f' of
         "undefined" -> pure Nothing
         "boolean" -> Just <<< Left <$> readImpl f'
         "object" -> Just <<< Right <$> readImpl f'
         _ -> fail $ ForeignError "Could not decode additionalProperties"

instance writeForeignSchema :: WriteForeign JsonSchema where
  writeImpl (JsonSchema s) = writeImpl
    (s { additionalProperties = (either writeImpl writeImpl) <$> s.additionalProperties }
    )

newtype SchemaRef = SchemaRef String
derive instance ntSchemaRef :: Newtype SchemaRef _
derive newtype instance eqSchemaRef :: Eq SchemaRef
derive newtype instance ordSchemaRef :: Ord SchemaRef
derive newtype instance showSchemaRef :: Show SchemaRef
derive newtype instance readForeignSchemaRef :: ReadForeign SchemaRef
derive newtype instance writeForeignSchemaRef :: WriteForeign SchemaRef

data TypeValidator = TypeValidatorString SchemaType | TypeValidatorArray (Array SchemaType)
derive instance eqTypeValidator :: Eq TypeValidator
derive instance ordTypeValidator :: Ord TypeValidator
instance writeForeignValidator :: WriteForeign TypeValidator where
  writeImpl = case _ of
    TypeValidatorString st -> writeImpl st
    TypeValidatorArray arr -> writeImpl arr
instance readForeignTypeValidator :: ReadForeign TypeValidator where
  readImpl f = parseArray <|> parseSingle
    where
      parseArray = TypeValidatorArray <$> (readImpl f :: F (Array SchemaType))
      parseSingle = TypeValidatorString <$> (readImpl f :: F SchemaType)
instance showTypeValidator :: Show TypeValidator where
  show (TypeValidatorArray arr) = show arr
  show (TypeValidatorString s) = show s

data SchemaType
  = SchemaObject
  | SchemaArray
  | SchemaString
  | SchemaNumber
  | SchemaInteger
  | SchemaBoolean
  | SchemaNull
derive instance eqSchemaType :: Eq SchemaType
derive instance ordSchemaType :: Ord SchemaType
instance writeForeignSchemaType :: WriteForeign SchemaType where
  writeImpl = case _ of
    SchemaObject  -> writeImpl "object"
    SchemaArray   -> writeImpl "array"
    SchemaString  -> writeImpl "string"
    SchemaNumber  -> writeImpl "number"
    SchemaInteger -> writeImpl "integer"
    SchemaBoolean -> writeImpl "boolean"
    SchemaNull    -> writeImpl "null"
instance readForeignSchemaType :: ReadForeign SchemaType where
  readImpl f = do
    decoded <- readString f
    case decoded of
      "object" -> pure SchemaObject
      "array" -> pure SchemaArray
      "string" -> pure SchemaString
      "number" -> pure SchemaNumber
      "integer" -> pure SchemaInteger
      "boolean" -> pure SchemaBoolean
      "null" -> pure SchemaNull
      _ -> fail (ForeignError $ "Could not decode value '" <> "' as type SchemaType")
instance showSchemaType :: Show SchemaType where
  show SchemaObject = "\"object\""
  show SchemaArray = "\"array\""
  show SchemaString = "\"string\""
  show SchemaNumber = "\"number\""
  show SchemaInteger = "\"integer\""
  show SchemaBoolean = "\"boolean\""
  show SchemaNull = "\"null\""
