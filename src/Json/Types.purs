module Json.Types where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Formatter.DateTime (formatDateTime, unformatDateTime)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype AtLeastOne a = AtLeastOne (NonEmptyArray a)
derive instance ntAtLeastOne :: Newtype (AtLeastOne a) _
derive newtype instance eqAtLeastOne :: Eq a => Eq (AtLeastOne a)
derive newtype instance ordAtLeastOne :: Ord a => Ord (AtLeastOne a)
derive newtype instance showAtLeastOne :: Show a => Show (AtLeastOne a)
instance readForeignAtLeastOne :: ReadForeign a => ReadForeign (AtLeastOne a) where
  readImpl x = do
    array <- readImpl x
    case NonEmptyArray.fromArray array of
      Nothing -> throwError (pure (ForeignError "Non-Empty array was empty"))
      Just nea -> pure (AtLeastOne nea)
instance writeForeignAtLeastOne :: WriteForeign a => WriteForeign (AtLeastOne a) where
  writeImpl (AtLeastOne nea) = writeImpl (NonEmptyArray.toArray nea)

newtype DateTimeRFC3339 = DateTimeRFC3339 DateTime
derive instance ntDateTimeRFC3339 :: Newtype DateTimeRFC3339 _
derive newtype instance eqDateTimeRFC3339 :: Eq DateTimeRFC3339
derive newtype instance ordDateTimeRFC3339 :: Ord DateTimeRFC3339
derive newtype instance showDateTimeRFC3339 :: Show DateTimeRFC3339
format :: String
format = "YYYY-MM-DDTHH:mm:ss.SSSZ"
instance readForeignDateTimeRFC3339 :: ReadForeign DateTimeRFC3339 where
  readImpl f = do
    s <- readImpl f
    case unformatDateTime format s of
        Left e -> throwError (pure (ForeignError e))
        Right v -> pure (DateTimeRFC3339 v)
instance writeForeignDateTimeRFC3339 :: WriteForeign DateTimeRFC3339 where
  writeImpl (DateTimeRFC3339 dt) =
    writeImpl $ either identity identity $ formatDateTime format dt