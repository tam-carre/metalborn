{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, FlexibleContexts,
             NoMonomorphismRestriction #-}

module App.InbornPower
  ( InbornAllo (..)
  , InbornFeru (..)
  , InbornPower (..)
  , ZeroOneOrEvery (..)
  ) where

import App.Metal    (Metal)
import Data.Default (Default (..))

----------------------------------------------------------------------------------------------------

data ZeroOneOrEvery a
  = Zero
  | One a
  | Every
  deriving (Eq, Show)

class InbornPower a where
  mkInborn ∷ ZeroOneOrEvery Metal → a

newtype InbornAllo
  = InbornAllo (ZeroOneOrEvery Metal)
  deriving (Eq, Generic, Show)

newtype InbornFeru
  = InbornFeru (ZeroOneOrEvery Metal)
  deriving (Eq, Generic, Show)

instance InbornPower InbornAllo where mkInborn = InbornAllo
instance InbornPower InbornFeru where mkInborn = InbornFeru
instance Default     InbornAllo where def      = mkInborn Zero
instance Default     InbornFeru where def      = mkInborn Zero

