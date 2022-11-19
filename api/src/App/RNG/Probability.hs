module App.RNG.Probability
  ( Probability
  , balanced
  , certain
  , impossible
  , parseProbability
  , unProb
  ) where

----------------------------------------------------------------------------------------------------

-- | A float between 0 and 1 inclusive
-- Note: you are allowed to use float literals to construct a Probability
-- be careful as the compiler doesn't ensure the literal is between 0 and 1
newtype Probability
  = ProbabilityNOEXPORT Float
  deriving (Eq, Fractional, Num, Show)

instance Bounded Probability where
  minBound = 0
  maxBound = 1

unProb ∷ Probability → Float
unProb (ProbabilityNOEXPORT float) = float

parseProbability ∷ MonadFail m ⇒ Float → m Probability
parseProbability x
  | 0 ≤ x ∧ x ≤ 1 = pure $ ProbabilityNOEXPORT x
  | otherwise     = fail $ "Expected number between 0 and 1 inclusive, got " ⊕ show x

certain ∷ Probability
certain = 1

impossible ∷ Probability
impossible = 0

balanced ∷ Probability
balanced = 0.5
