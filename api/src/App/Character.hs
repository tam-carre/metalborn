{-# LANGUAGE TemplateHaskell #-}

module App.Character (Character (..), mkCharacter) where

import App.Character.Abilities   (Abilities, AbilityProbabilities, mkAbilities)
import App.Character.Description (DescriptionBlock (..), describeAbilities, seed)
import App.Character.Name        (Name (..))
import App.Gender                (Gender (..))
import Data.Default              (Default (..))
import Servant.Docs              (ToSample (..), singleSample)
import Servant.Elm               (defaultOptions, deriveBoth)

----------------------------------------------------------------------------------------------------

data Character
  = Character Name Gender Abilities [DescriptionBlock]
  deriving (Eq, Generic, Show)

deriveBoth defaultOptions ''Character

instance ToSample Character where
  toSamples _ = singleSample $ mkCharacter (Name "Kaladin") Male def

mkCharacter ∷ Name → Gender → AbilityProbabilities → Character
mkCharacter name gender probabs =
  let abilitiesGen      = seed name gender
      abilities         = evalState (mkAbilities probabs) abilitiesGen
      descriptionBlocks = describeAbilities name gender abilities
  in Character name gender abilities descriptionBlocks
