{-# LANGUAGE TemplateHaskell, OverloadedRecordDot #-}

module App.Character.Abilities
  ( Abilities (..)
  , AbilitiesObtained (..)
  , AbilityProbabilities (..)
  , mkAbilities
  ) where

import App.Character.Metalborn (Halfborn (..), Metal, Metalborn (..), Singleborn (..), mkTwinborn)
import App.RNG.Probability     (Probability)
import App.RNG.Rand            (Rand, coinflip, exponentiallyRarer, rand, randBool, randomlySplit)
import Data.Default            (Default (..))
import Servant.Elm             (deriveBoth, defaultOptions)

----------------------------------------------------------------------------------------------------

data Abilities
  = Abilities (Maybe Metalborn) AbilitiesObtained
  deriving (Generic, Show)

instance Default Abilities where
  def = Abilities Nothing def

data AbilitiesObtained
  = AbilitiesObtained
    { spikedA ∷ [Metal]
    , spikedF ∷ [Metal]
    , medallA ∷ [Metal]
    , medallF ∷ [Metal]
    , grenade ∷ Bool
    }
  deriving (Generic, Show)

instance Default AbilitiesObtained where
  def = AbilitiesObtained [] [] [] [] False

data AbilityProbabilities
  = AbilityProbabilities
    { metalborn ∷ Probability
    , twinborn  ∷ Probability
    , fullPower ∷ Probability
    , spike     ∷ Probability
    , medall    ∷ Probability
    , grenade   ∷ Probability
    }
  deriving (Eq, Generic, Show)

deriveBoth defaultOptions ''AbilityProbabilities

instance Default AbilityProbabilities where
  def = AbilityProbabilities
    { metalborn = 1
    , twinborn  = 0.8
    , fullPower = 0.01
    , spike     = 0.05
    , medall    = 0.05
    , grenade   = 0.25
    }

mkAbilities ∷ AbilityProbabilities → Rand Abilities
mkAbilities ps = Abilities <$> mkMetalborn ps <*> mkAbilitiesObtained ps

mkMetalborn ∷ AbilityProbabilities → Rand (Maybe Metalborn)
mkMetalborn ps = do
  isMetalborn ← randBool ps.metalborn
  isTwinborn  ← randBool ps.twinborn  <&> (∧ isMetalborn)
  isFull1     ← randBool ps.fullPower <&> (∧ isMetalborn)
  isFull2     ← randBool ps.fullPower <&> (∧ isTwinborn)
  misting     ← rand
  ferring     ← rand

  let mistborn    = Halfborn $ Mistborn    (justIf isTwinborn ferring)
      feruchemist = Halfborn $ Feruchemist (justIf isTwinborn misting)
      twinborn    = mkTwinborn misting ferring

  (halfborn, singleborn) ← coinflip (mistborn,    Singleborn $ Misting misting)
                                    (feruchemist, Singleborn $ Ferring ferring)
  pure $
    case (isFull1, isFull2, isTwinborn, isMetalborn) of
         (True,    True,    _,          _          ) → Just Fullborn
         (True,    _,       _,          _          ) → Just halfborn
         (_,       _,       True,       _          ) → Just twinborn
         (_,       _,       _,          True       ) → Just singleborn
         _                                           → Nothing

mkAbilitiesObtained ∷ AbilityProbabilities → Rand AbilitiesObtained
mkAbilitiesObtained ps = do
  spikeNum  ← exponentiallyRarer ps.spike
  medallNum ← exponentiallyRarer ps.medall
  spikes    ← sequence $ rand <$ drop 1 [0..spikeNum]
  medalls   ← sequence $ rand <$ drop 1 [0..medallNum]
  grenade   ← randBool ps.grenade
  (spikedA, spikedF) ← randomlySplit spikes
  (medallA, medallF) ← randomlySplit medalls

  pure $ AbilitiesObtained { spikedA, spikedF, medallA, medallF, grenade }
