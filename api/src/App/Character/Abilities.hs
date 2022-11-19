{-# LANGUAGE NoMonomorphismRestriction, OverloadedRecordDot #-}

module App.Character.Abilities (Abilities (..), AbilityProbabilities (..), mkAbilities) where

import App.Character.Gender        (Gender (..))
import App.Character.GenderNeutral (txt)
import App.Character.Metalborn     (Halfborn (..), Metal, Metalborn (..), Singleborn (..),
                                    mkTwinborn)
import App.Character.Name          (Name)
import App.RNG.Probability         (Probability)
import App.RNG.Rand                (Rand, coinflip, exponentiallyRarer, rand, randBool, randomEnum,
                                    randomEnumR, randomlySplit)
import App.Utils                   (justIf, pp)
import Control.Lens                ((.~), (^.))
import Data.Default                (Default (..))
import Relude.Extra.Newtype        (un)
import System.Random               (Random (..))

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
    , medailA ∷ [Metal]
    , medailF ∷ [Metal]
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
    , medail    ∷ Probability
    , grenade   ∷ Probability
    }
  deriving (Generic, Show)

instance Default AbilityProbabilities where
  def = AbilityProbabilities
    { metalborn = 1
    , twinborn  = 0.8
    , fullPower = 0.01
    , spike     = 0.05
    , medail    = 0.05
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
  medailNum ← exponentiallyRarer ps.medail
  spikes    ← sequence $ rand <$ drop 1 [0..spikeNum]
  medails   ← sequence $ rand <$ drop 1 [0..medailNum]
  grenade   ← randBool ps.grenade
  (spikedA, spikedF) ← randomlySplit spikes
  (medailA, medailF) ← randomlySplit medails

  pure $ AbilitiesObtained { spikedA, spikedF, medailA, medailF, grenade }

----------------------------------------------------------------------------------------------------
-- Description

-- sentence ∷ ( → ) →
-- sentence builder =

-- describeAbilities ∷ Name → Gender → Abilities → Text
-- describeAbilities name gender abilities =
  -- let x = name & unInborn
  -- in
  -- case metalbornKinds abilities.inbornA abilities.inbornF of
    -- Nothing → txt ( name ^. un ) ⊕ "doesn't have any inborn affinity with the Metallic Arts."

-- primaryTitles
  -- ∷ (MetalbornAndReason, MetalbornAndReason, MetalbornAndReason)
  -- → [Metalborn]
-- primaryTitles = \case
  -- (Just a, Nothing, Nothing) → [a.name]
  -- (Nothing, Just f, Nothing) → [f.name]
  -- (Nothing, Nothing, Just t) → [] -- illogical state

-- my_foldr ∷ (a → b → b) → b → [a] → b
-- my_foldr = _
