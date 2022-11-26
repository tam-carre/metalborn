module Config (Config (..), appConf) where

import Database.PostgreSQL.Simple (ConnectInfo (..))

import Secrets qualified
-- ^ Not version controlled obv

----------------------------------------------------------------------------------------------------

-- ATM config is hardcoded

data Config
  = Config
    { pg   ∷ ConnectInfo
    , todo ∷ Int
    }
  deriving (Generic)

appConf ∷ Config
appConf = Config
  { pg = ConnectInfo Secrets.pgHost 5432 "tam-carre" Secrets.pgPw "tam-carre/metalborn"
  , todo = 42
  }
