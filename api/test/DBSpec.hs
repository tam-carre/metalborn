module DBSpec (spec) where

import App                       (App, AppError (..), DBError (..), runApp')
import App.Character             (Character (..))
import App.Character.Abilities   (Abilities (..), AbilitiesObtained (..))
import App.Character.Description (DescriptionBlock (..))
import App.Character.Metalborn   (Ferring (..), Halfborn (..), Metal (..), Metalborn (..))
import App.Character.Name        (Name (Name))
import App.DB                    qualified as DB
import App.Gender                (Gender (Male))
import Test.Hspec                (Spec, beforeAll, it, shouldBe)

----------------------------------------------------------------------------------------------------

-- | These effectful integration tests are run against whatever database is set in Config.hs
-- If one fails, the remaining ones will not be run.
spec ∷ Spec
spec = beforeAll (newIORef PreviousTestsSuccessful) do
  let dummyName   = Name "TEST_CHARACTER_NAME_FSWTRCRSPTWF"
      dummyGender = Male
      dummyBlocks = [AllomancyBlock "dummy allo", FeruchemyBlock "dummy feru"]
      dummyAbils  = Abilities (Just . Halfborn . Mistborn $ Just Skimmer)
                              (AbilitiesObtained [Steel] [] [] True)
      dummyChara  = Character dummyName dummyGender dummyAbils dummyBlocks

  it "getCharacter should successfully query a nonexistent char, but retrieve Nothing" .
    expectWithShortCircuit (Right Nothing) $
      DB.getCharacter dummyName dummyGender

  it "createCharacter should successfully create a character with description" .
    expectWithShortCircuit (Right ()) $
      DB.createCharacter dummyChara

  it "getCharacter should successfully retrieve the char we just created" .
    expectWithShortCircuit (Right (Just dummyChara)) $
      DB.getCharacter dummyName dummyGender

  it "getCharacter should safely report UniqueConstraintViolated if creating a dupe" .
    expectWithShortCircuit (Left (DBError UniqueConstraintViolated)) $
      DB.createCharacter dummyChara

  it "deleteCharacter should successfully delete the char we just created" .
    expectWithShortCircuit (Right ()) $
      DB.deleteCharacter dummyName dummyGender

  it "getCharacter should now retrieve Nothing when trying to get the deleted char" .
    expectWithShortCircuit (Right Nothing) $
      DB.getCharacter dummyName dummyGender

data PreviousTestsStatus = PreviousTestsSuccessful | PreviousTestsFailed deriving (Eq, Show)

-- | Helper to prevent effectful or expensive tests from running if previous
-- tests failed
expectWithShortCircuit ∷ (Eq a, Show a) ⇒
  Either AppError a → App a → IORef PreviousTestsStatus → IO ()
expectWithShortCircuit expected app ref =
  do
    status ← readIORef ref
    status `shouldBe` PreviousTestsSuccessful

    appResult ← runApp' app
    when (appResult ≢ expected) $ writeIORef ref PreviousTestsFailed
    appResult `shouldBe` expected
