{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables #-}

module App.Elm (main, generateElmCode) where

import App.Character             (Character)
import App.Character.Abilities   (Abilities, AbilitiesObtained, AbilityProbabilities)
import App.Character.Description (DescriptionBlock)
import App.Character.Metalborn   (Ferring, Halfborn, Metal, Metalborn, Misting, Singleborn,
                                  Twinborn)
import App.Character.Name        (Name)
import App.Gender                (Gender)
import App.RNG.Probability       (Probability)
import App.Server                (siteAPI)
import Elm.TyRep                 (IsElmDefinition)
import Servant.Elm               (DefineElm (..), defElmImports, generateElmModule)

----------------------------------------------------------------------------------------------------

main ∷ IO ()
main = generateElmCode

generateElmCode ∷ IO ()
generateElmCode = generateElmModule [ "API" ] defElmImports "../webapp/src" sharedTypes siteAPI

sharedTypes ∷ [DefineElm]
sharedTypes =
  [ share @Abilities
  , share @AbilitiesObtained
  , share @AbilityProbabilities
  , share @Character
  , share @DescriptionBlock
  , share @Ferring
  , share @Gender
  , share @Halfborn
  , share @Metal
  , share @Metalborn
  , share @Misting
  , share @Name
  , share @Probability
  , share @Singleborn
  , share @Twinborn
  ] where
  share ∷ ∀ a . IsElmDefinition a ⇒ DefineElm
  share = DefineElm (Proxy @a)
