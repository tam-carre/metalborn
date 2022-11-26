module CharacterDescriptionSpec (spec) where

import App.Character.Abilities   (mkAbilities)
import App.Character.Description (describeAbilities, seed)
import App.RNG.Rand              (rand)
import Data.Default              (Default (def))
import Data.Text                 qualified as T
import System.Random             (mkStdGen)
import Test.Hspec                (Spec, it, shouldBe)

----------------------------------------------------------------------------------------------------

spec ∷ Spec
spec = do
  it "(dummy test, we save sample Character descriptions to testDescriptions.txt for **MANUAL** verification)" $ do
    let separator = "\n\n≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡\n\n"
        descriptions = map
          ( (T.intercalate "\n\n" . map (bracketFstWrd . show))
          . (\(name, gn) → describeAbilities name gn . evalState (mkAbilities def) $ seed name gn)
          . (\gen → (evalState rand gen, evalState rand gen))
          . mkStdGen
          )
          [0..2000]

    writeFile "./test/testDescriptions.txt" (toString $ T.intercalate separator descriptions)

    True `shouldBe` True

bracketFstWrd ∷ Text → Text
bracketFstWrd = unwords . brackFst . words where
  brackFst []     = []
  brackFst (x:xs) = ("[" ⊕ x ⊕ "]") : xs
