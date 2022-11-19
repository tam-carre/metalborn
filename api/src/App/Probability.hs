module App.Probability
  ( Probability
  , balanced
  , certain
  , impossible
  , p
  , parseProbability
  , unProb
  ) where

-- | A float between 0 and 1 inclusive
newtype Probability
  = ProbabilityNOEXPORT Float
  deriving (Eq, Show)

-- | UNSAFE, only use for literals! must be between 0 and 1 inclusive
p ∷ Float → Probability
p = ProbabilityNOEXPORT

unProb ∷ Probability → Float
unProb (ProbabilityNOEXPORT float) = float

parseProbability ∷ MonadFail m ⇒ Float → m Probability
parseProbability x
  | 0 ≤ x ∧ x ≤ 1 = pure $ p x
  | otherwise     = fail $ "Expected number between 0 and 1 inclusive, got " ⊕ show x

certain ∷ Probability
certain = p 1

impossible ∷ Probability
impossible = p 0

balanced ∷ Probability
balanced = p 0.5
