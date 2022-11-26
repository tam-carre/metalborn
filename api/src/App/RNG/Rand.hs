{-# LANGUAGE ScopedTypeVariables #-}

-- | Random functions in a State StdGen monad
module App.RNG.Rand
  ( Rand
  , RandT
  , coinflip
  , exponentiallyRarer
  , rand
  , randBool
  , randEl
  , randR
  , randWith
  , randomEl
  , randomEnum
  , randomEnumR
  , randomlySplit
  , toRand
  , toRandIO
  ) where

import App.RNG.Probability (Probability, balanced, unProb)
import Control.Lens        (Field1 (_1), both, (%~))
import Data.List.NonEmpty  qualified as NE
import System.Random       (Random (random, randomR), RandomGen, StdGen, initStdGen)

----------------------------------------------------------------------------------------------------

-- | System.Random pure function signatures
type StockRandom g a = g → (a, g)

type Rand  = RandT Identity

type RandT = StateT StdGen

rand ∷ (Random r) ⇒ Rand r
rand = state random

randWith ∷ Random r ⇒ StdGen → r
randWith = evalState rand

randR ∷ (Random r) ⇒ (r, r) → Rand r
randR = state . randomR

randBool ∷ Probability → Rand Bool
randBool (unProb → p) = rand @Float <&> (≤ p)

randEl ∷ NonEmpty a → Rand a
randEl = state . randomEl

coinflip ∷ a → a → Rand a
coinflip a b = bool a b <$> randBool balanced

-- e.g. if I have 0.05 likelihood of owning a single spike, how many spikes do I have?
exponentiallyRarer ∷ Probability → Rand Int
exponentiallyRarer prob = randBool prob ≫= \case
  False → pure 0
  True  → (1 +) <$> exponentiallyRarer prob

randomlySplit ∷ [a] → Rand ([a], [a])
randomlySplit xs = splitAt <$> randR (0, length xs) ?? xs

randomEl ∷ RandomGen g ⇒ NonEmpty a → StockRandom g a
randomEl xs = randomR (0, length xs - 1) ⋙ _1 %~ (xs NE.!!)

randomEnum ∷ ∀ a g . (Enum a, Bounded a, RandomGen g) ⇒ StockRandom g a
randomEnum = randomR bounds ⋙ _1 %~ toEnum where
  bounds = (minBound @a, maxBound) & both %~ fromEnum

randomEnumR ∷ (Enum a, RandomGen g) ⇒ (a, a) → StockRandom g a
randomEnumR bounds = randomR (bounds & both %~ fromEnum) ⋙ _1 %~ toEnum

toRand ∷ Random a ⇒ Maybe a → Rand a
toRand = maybe rand pure

toRandIO ∷ (Random a, MonadIO m) ⇒ Maybe a → m a
toRandIO maybeA = evalState (toRand maybeA) <$> initStdGen
