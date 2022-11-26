module Main (main) where

import App                       (runServer)
import App.Character             (Character (..), mkCharacter)
import App.Character.Abilities   (AbilityProbabilities)
import App.Character.Description (DescriptionBlock)
import App.Character.Name        (Name, parseIfPresentElseRand)
import App.DB                    qualified as DB
import App.Gender                (Gender)
import App.RNG.Probability       (Probability)
import App.RNG.Rand              (toRandIO)
import Data.Default              (Default (..))
import Servant                   (JSON, Post, ReqBody, type (:<|>) (..), type (:>))
import Servant.Elm               (DefineElm (..), defElmImports, generateElmModule)

----------------------------------------------------------------------------------------------------

type SiteAPI = "api" :>
    ( "character"
        :> ReqBody '[JSON] (Maybe Text, Maybe Gender)
        :> Post '[JSON] Character
 :<|> "random_character"
        :> ReqBody '[JSON] (Maybe Text, Maybe Gender, AbilityProbabilities)
        :> Post '[JSON] Character
    )

main ∷ IO ()
main = runElmCodegen ≫ runServer 8081 (Proxy @SiteAPI) server where
  runElmCodegen =
    generateElmModule [ "API" ] defElmImports "../webapp/src"
      [ DefineElm (Proxy @AbilityProbabilities)
      , DefineElm (Proxy @Character)
      , DefineElm (Proxy @DescriptionBlock)
      , DefineElm (Proxy @Gender)
      , DefineElm (Proxy @Name)
      , DefineElm (Proxy @Probability)
      ]
      (Proxy @SiteAPI)

  server = getCharacter :<|> postCharacter where
    getCharacter (maybeNameTxt, maybeGender) = do
      name   ← parseIfPresentElseRand maybeNameTxt
      gender ← toRandIO maybeGender
      DB.getCharacter name gender ≫= \case
        Just character → pure character
        Nothing → do
          let newCharacter = mkCharacter name gender def
          DB.createCharacter newCharacter
          pure newCharacter

    postCharacter (maybeNameTxt, maybeGender, probabs) = do
      name   ← parseIfPresentElseRand maybeNameTxt
      gender ← toRandIO maybeGender
      pure $ mkCharacter name gender probabs
