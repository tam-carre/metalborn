{-# LANGUAGE TemplateHaskell #-}

module App.RNG.Probability (Probability, balanced, unProb) where

import Data.Aeson      (FromJSON (..), ToJSON (..), Value (..))
import Data.Scientific (fromFloatDigits, toRealFloat)
import Servant.Elm     (defaultOptions, deriveElmDef)

----------------------------------------------------------------------------------------------------

-- | A float between 0 and 1 inclusive
-- Note: you are allowed to use float literals to construct a Probability
-- be careful as the compiler doesn't ensure the literal is between 0 and 1
newtype Probability
  = ProbabilityNOEXPORT Float
  deriving (Eq, Fractional, Num, Show)

deriveElmDef defaultOptions ''Probability

instance FromJSON Probability where
  parseJSON (Number n) | 0 ≤ n ∧ n ≤ 1 = pure . ProbabilityNOEXPORT $ toRealFloat n
                       | otherwise     = fail "number not between 0 and 1"
  parseJSON _ = fail "not a number"

instance ToJSON Probability where
  toJSON (ProbabilityNOEXPORT p) = Number $ fromFloatDigits p

instance Bounded Probability where
  minBound = 0
  maxBound = 1

unProb ∷ Probability → Float
unProb (ProbabilityNOEXPORT float) = float

balanced ∷ Probability
balanced = 0.5
