module Nakadi.Minimal where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Json.Schema (JsonSchema(..))
import Nakadi.Types (EventType, EventTypeName, EventTypeSchema, OwningApplication, StringSchema(..), Subscription, StreamParameters)

eventType :: EventTypeName -> OwningApplication -> EventTypeSchema -> EventType
eventType name owningApplication schema =
  { name
  , owning_application: owningApplication
  , category: "undefined"
  , audience: Nothing
  , authorization: Nothing
  , cleanup_policy: Nothing
  , compatibility_mode: Nothing
  , created_at: Nothing
  , default_statistic: Nothing
  , enrichment_strategies: Nothing
  , options: Nothing
  , ordering_instance_ids: Nothing
  , ordering_key_fields: Nothing
  , partition_key_fields: Nothing
  , partition_strategy: Nothing
  , updated_at: Nothing
  , schema
  }

eventTypeSchema :: EventTypeSchema
eventTypeSchema =
  { "type": "json_schema"
  , schema
  , created_at: Nothing
  , version: Nothing
  }
  where
    schema = ValidStringSchema (JsonSchema
      { "type": Nothing
      , additionalProperties: (Just <<< Left) true
      , description : Nothing
      , format : Nothing
      , items : Nothing
      , oneOf : Nothing
      , properties : Nothing
      , ref : Nothing
      , required : Nothing
      }
    )

subscription :: OwningApplication -> EventTypeName -> Subscription
subscription owning_application eventName =
  { id: Nothing
  , owning_application
  , event_types: [eventName]
  , consumer_group: Nothing
  , created_at: Nothing
  , read_from: Nothing
  , initial_cursors: Nothing
  , status: Nothing
  , authorization: Nothing
  }

streamParameters :: Int -> Int -> StreamParameters
streamParameters batchLimit maxUncommitted =
  { partitions: Nothing
  , max_uncommitted_events: Just maxUncommitted
  , batch_limit: Just batchLimit
  , stream_limit: Nothing
  , batch_flush_timeout: Nothing
  , stream_timeout: Nothing
  , commit_timeout: Nothing
  }
