module Node.Stream.Util
  ( writable
  , Done (..)
  , throughMap
  , splitWithFn
  )
  where

import Prelude

import Effect (Effect)
import Node.Encoding (Encoding)
import Node.Stream (Writable, Duplex)

type Done = Effect Unit
type Next a = a -> Effect Unit

foreign import writableImpl
  ∷ ∀ a w. (a -> Encoding -> Done -> Effect Unit) -> Effect (Writable w)

writable
  ∷ ∀ a w
  . (a -> Encoding -> Done -> Effect Unit)
 -> Effect (Writable w)
writable = writableImpl

foreign import throughMap
  ∷ ∀ a b. (a -> b) -> Effect Duplex

foreign import split2 :: forall a. (String -> a) -> Effect Duplex

splitWithFn = split2
