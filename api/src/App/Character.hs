{-# LANGUAGE OverloadedRecordDot #-}

module App.Character (Character (..)) where

import App.Character.Abilities (Abilities)
import App.Gender    (Gender)
import App.Character.Name      (Name (..))
import App.RNG.Rand            (Rand, randomEnum, randomEnumR, toRand)
import Relude.Unsafe           qualified as Unsafe
import System.Random           (Random (..), StdGen, mkStdGen)

----------------------------------------------------------------------------------------------------

data Character
  = Character
    { id        ∷ Text
    , name      ∷ Name
    , gender    ∷ Gender
    , abilities ∷ Abilities
      -- , abilityExplanation ∷ AbilityExplanation -- not sure if needed, might
      -- have a 1:1 Abilities → Explanation relation?
    , backstory ∷ Backstory
    }

data CharacterUserInput
  = CharacterUserInput
    { name   ∷ Maybe Name
    , gender ∷ Maybe Gender
    }

data CharacterInput
  = CharacterInput
    { name   ∷ Name
    , gender ∷ Gender
    }

fillCharacterInput ∷ CharacterUserInput → Rand CharacterInput
fillCharacterInput maybes = CharacterInput <$> toRand maybes.name <*> toRand maybes.gender

-- mkCharacter ∷ CharacterInput → Character
-- mkCharacter = _

-- | A character's name and gender determine the RNG seed
seed ∷ Name → Gender → StdGen
seed (Name name) gender = mkStdGen (nameAsInt + genderAsInt) where
  nameAsInt   = Unsafe.read . concatMap (show . ord) $ toString name
  genderAsInt = fromEnum gender

data Backstory
  = Backstory
    { job                ∷ Job
    , worldhopper        ∷ Bool
    , earlyLife          ∷ EarlyLife
    , crisis             ∷ Crisis
    , partners           ∷ PartnerInfo
    , abilitiesAnecdotes ∷ [AbilitiesAnecdotes]
    }

data Job = Constable | Scholar | BountyHunter

data EarlyLife = AllomancerFamily | Slums | Prodigy

data Crisis = TookDownTerroristOrg | LostFaithInHarmony

data PartnerInfo
  = NoPartner
  | OnePartner Partner
  | InTeam Team

data Partner = Partner

data Team = Team

data AbilitiesAnecdotes = AbilitiesAnecdotes
