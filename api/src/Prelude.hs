module Prelude
  ( module Control.Arrow.Unicode
  , module Control.Monad.Unicode
  , module Data.Bool.Unicode
  , module Data.Eq.Unicode
  , module Data.List.Unicode
  , module Data.Monoid.Unicode
  , module Data.Ord.Unicode
  , module Relude
  , echo
  ) where

import Control.Arrow.Unicode
import Control.Monad.Unicode
import Data.Bool.Unicode
import Data.Eq.Unicode
import Data.Generics.Labels  ()
import Data.List.Unicode
import Data.Monoid.Unicode
import Data.Ord.Unicode
import Relude

----------------------------------------------------------------------------------------------------

-- | Short name for putTextLn
echo ∷ MonadIO m ⇒ Text → m ()
echo = putTextLn
