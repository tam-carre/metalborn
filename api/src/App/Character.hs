{-# LANGUAGE OverloadedRecordDot #-}

module App.Character (Character (..), CharacterInput (..), fillCharacterInput, seed) where

import App.Character.Abilities   (Abilities)
import App.Character.Description (DescriptionBlock)
import App.Character.Name        (Name (..))
import App.Gender                (Gender)
import App.RNG.Rand              (Rand, randomEnum, randomEnumR, toRand)
import Data.Default              (Default (..))
import Relude.Unsafe             qualified as Unsafe
import System.Random             (Random (..), StdGen, mkStdGen)

----------------------------------------------------------------------------------------------------

data Character
  = Character
    { name        ∷ Name
    , gender      ∷ Gender
    , description ∷ [DescriptionBlock]
    }
  deriving (Eq, Generic, Show)

data CharacterUserInput
  = CharacterUserInput
    { name   ∷ Maybe Name
    , gender ∷ Maybe Gender
    }

instance Default CharacterUserInput where
  def = CharacterUserInput Nothing Nothing

data CharacterInput
  = CharacterInput
    { name   ∷ Name
    , gender ∷ Gender
    }

fillCharacterInput ∷ CharacterUserInput → Rand CharacterInput
fillCharacterInput maybes = CharacterInput <$> toRand maybes.name <*> toRand maybes.gender

-- | A character's name and gender determine the RNG seed
seed ∷ Name → Gender → StdGen
seed (Name name) gender = mkStdGen (nameAsInt + genderAsInt) where
  nameAsInt   = Unsafe.read . concatMap (show . ord) $ toString name
  genderAsInt = fromEnum gender
