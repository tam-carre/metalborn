module App.Server (SiteAPI, runServer, siteAPI) where

import App                      (AppError (BadRequestError), runApp')
import App.Character            (Character (..), mkCharacter)
import App.Character.Abilities  (AbilityProbabilities)
import App.Character.Name       (parseIfPresentElseRand)
import App.DB                   qualified as DB
import App.Gender               (Gender)
import App.RNG.Rand             (toRandIO)
import Data.Default             (Default (..))
import Network.Wai.Handler.Warp (Port, run)
import Servant                  (JSON, Post, ReqBody, err400, err500, hoistServer, serve,
                                 type (:<|>) (..), type (:>))

----------------------------------------------------------------------------------------------------

type SiteAPI =
  "api"
    :> ( "character"
           :> ReqBody '[JSON] (Maybe Text, Maybe Gender)
           :> Post '[JSON] Character
       :<|>
         "randomCharacter"
           :> ReqBody '[JSON] (Maybe Text, Maybe Gender, AbilityProbabilities)
           :> Post '[JSON] Character
       )

siteAPI ∷ Proxy SiteAPI
siteAPI = Proxy

runServer ∷ Port → IO ()
runServer port =
  run port . serve siteAPI $ hoistServer siteAPI appToServantHandler server
  where
  appToServantHandler = runApp' ↣ \case
    Right val            → pure val
    Left BadRequestError → throwError err400
    Left otherErr        → print otherErr ≫ throwError err500

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
