module CharacterDescriptionSpec (spec) where

import App.Character             (CharacterInput (CharacterInput), fillCharacterInput, seed)
import App.Character.Abilities   (mkAbilities)
import App.Character.Description (describeAbilities)
import Data.Default              (Default (def))
import Data.Text                 qualified as T
import System.Random             (mkStdGen)
import Test.Hspec                (Spec, describe, it, shouldBe)

----------------------------------------------------------------------------------------------------

spec ∷ Spec
spec = do
  describe "Character description generator" $ do
    it "(dummy test, we save sample Character descriptions to testDescriptions.txt for **MANUAL** verification)" $ do
      let separator = "\n\n≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡\n\n"
          descriptions = [0..2000]
            <&> evalState (fillCharacterInput def) . mkStdGen
            <&> (\(CharacterInput name gend) → (seed name gend, name, gend))
            <&> (\(gen, name', gend') → evaluatingState gen
                                      . describeAbilities name' gend'
                                      $ evalState (mkAbilities def) gen
                )
            <&> (T.intercalate "\n\n" . map (bracketFstWrd . show))

      writeFile "./test/testDescriptions.txt" (toString $ T.intercalate separator descriptions)

      True `shouldBe` True

bracketFstWrd ∷ Text → Text
bracketFstWrd = unwords . brackFst . words where
  brackFst []     = []
  brackFst (x:xs) = ("[" ⊕ x ⊕ "]") : xs
