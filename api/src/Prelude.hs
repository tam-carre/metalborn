module Prelude
  ( module Control.Arrow.Unicode
  , module Control.Monad.Unicode
  , module Data.Bool.Unicode
  , module Data.Eq.Unicode
  , module Data.List.Unicode
  , module Data.Monoid.Unicode
  , module Relude
  , echo
  ) where

import Relude hiding (id)

import Control.Arrow.Unicode
import Control.Monad.Unicode
import Data.Bool.Unicode
import Data.Eq.Unicode
import Data.List.Unicode
import Data.Monoid.Unicode

----------------------------------------------------------------------------------------------------

-- | Short name for putTextLn
echo ∷ MonadIO m ⇒ Text → m ()
echo = putTextLn
