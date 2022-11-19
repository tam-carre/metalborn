{-# LANGUAGE DataKinds, DeriveGeneric, DuplicateRecordFields, FlexibleContexts,
             NoMonomorphismRestriction, OverloadedLabels, OverloadedRecordDot, TypeApplications,
             ViewPatterns #-}

module App.CharacterGenerator (mkAbilities) where

import App.Character   (Abilities (..))
import App.InbornPower (InbornPower (mkInborn), ZeroOneOrEvery (..))
import App.Metal       (Metal (..))
import App.Probability (Probability, balanced, p, unProb)
import Control.Lens    ((.~))
import Data.Default    (Default (..))
import Relude.Extra    (safeToEnum)
import System.Random   (Random (random, randomR), StdGen)

----------------------------------------------------------------------------------------------------

data GeneratorCtx
  = GeneratorCtx
    { randomGen     ∷ StdGen
    , probabilities ∷ AbilityProbabilities
    }
  deriving (Generic, Show)

type Generator = Reader GeneratorCtx

data AbilityProbabilities
  = AbilityProbabilities
    { fstInborn ∷ Probability
    , sndInborn ∷ Probability
    , fullPower ∷ Probability
    , spike     ∷ Probability
    , medail    ∷ Probability
    }
  deriving (Generic, Show)

instance Default AbilityProbabilities where
  def = AbilityProbabilities
    { fstInborn = p 1
    , sndInborn = p 0.8
    , fullPower = p 0.01
    , spike     = p 0.05
    , medail    = p 0.05
    }

mkAbilities ∷ Generator Abilities
mkAbilities = addSpikesAndMedails =≪ addInbornAbilities def

addInbornAbilities ∷ Abilities → Generator Abilities
addInbornAbilities abils = do
  probabs  ← asks (.probabilities)
  hasAbty1 ← randBool probabs.fstInborn
  hasAbty2 ← randBool probabs.sndInborn <&> (∧ hasAbty1)
  metal1   ← randMetal
  metal2   ← randMetal
  isFull1  ← randBool probabs.fullPower <&> (∧ hasAbty1)
  isFull2  ← randBool probabs.fullPower <&> (∧ hasAbty2)

  let mk isFull cond metal | isFull    = mkInborn Every
                           | cond      = mkInborn (One metal)
                           | otherwise = mkInborn Zero
      asAbty1 = mk isFull1 hasAbty1 metal1
      asAbty2 = mk isFull2 hasAbty2 metal2

  coinflip
    (abils & #inbornA .~ asAbty1 & #inbornF .~ asAbty2)
    (abils & #inbornF .~ asAbty1 & #inbornA .~ asAbty2)

addSpikesAndMedails ∷ Abilities → Generator Abilities
addSpikesAndMedails abils = do
  probabs   ← asks (.probabilities)
  spikeNum  ← exponentiallyRarer probabs.spike
  medailNum ← exponentiallyRarer probabs.medail
  spikes    ← traverse (const randMetal) [1..spikeNum]
  medails   ← traverse (const randMetal) [1..medailNum]

  (spikedA, spikedF) ← randomlySplit spikes
  (medailA, medailF) ← randomlySplit medails

  pure $ abils
    & #spikedA .~ spikedA
    & #spikedF .~ spikedF
    & #medailA .~ medailA
    & #medailF .~ medailF

randBool ∷ Probability → Generator Bool
randBool (unProb → prob) = rand @Float <&> (≤ prob)

coinflip ∷ a → a → Generator a
coinflip a b = randBool balanced <&> \win → if win then a else b

rand ∷ Random r ⇒ Generator r
rand = fst . random <$> gen

randR ∷ Random r ⇒ (r, r) → Generator r
randR (lo, hi) = fst . randomR (lo, hi) <$> gen

gen ∷ Generator StdGen
gen = asks (.randomGen)

randMetal ∷ Generator Metal
randMetal = randR (0,1) <&> safeToEnum @Metal <&> fromMaybe Aluminum

-- e.g. if I have 0.05 likelihood of owning a single spike, how many spikes do I have?
exponentiallyRarer ∷ Probability → Generator Int
exponentiallyRarer prob = do
  mustAddOne ← randBool prob
  if mustAddOne
     then (1 +) <$> exponentiallyRarer prob
     else pure 0

randomlySplit ∷ [a] → Generator ([a], [a])
randomlySplit xs = do
  nth ← randR (0, length xs)
  pure $ splitAt nth xs
