{-# LANGUAGE TemplateHaskell #-}

module App.Character (Character (..), mkCharacter) where

import App.Character.Abilities   (AbilityProbabilities, mkAbilities)
import App.Character.Description (DescriptionBlock (..), describeAbilities, seed)
import App.Character.Name        (Name (..))
import App.Gender                (Gender (..))
import Servant.Elm (deriveBoth, defaultOptions)

----------------------------------------------------------------------------------------------------

data Character
  = Character
    { name        ∷ Name
    , gender      ∷ Gender
    , description ∷ [DescriptionBlock]
    }
  deriving (Eq, Generic, Show)

deriveBoth defaultOptions ''Character

mkCharacter ∷ Name → Gender → AbilityProbabilities → Character
mkCharacter name gender probabs =
  let abilitiesGen      = seed name gender
      abilities         = evalState (mkAbilities probabs) abilitiesGen
      descriptionBlocks = describeAbilities name gender abilities
  in Character name gender descriptionBlocks
