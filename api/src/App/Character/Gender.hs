module App.Character.Gender (Gender (..)) where

import App.RNG.Rand  (randomEnum, randomEnumR)
import System.Random (Random (..))

----------------------------------------------------------------------------------------------------

data Gender = Male | Female | Other deriving (Bounded, Enum, Eq)

instance Random Gender where
  randomR = randomEnumR
  random  = randomEnum
