module Config (Config (..), appConf) where

import Database.PostgreSQL.Simple (ConnectInfo (..))

----------------------------------------------------------------------------------------------------

-- ATM config is hardcoded

newtype Config
  = Config { pg ∷ ConnectInfo }
  deriving (Generic)

appConf ∷ Config
appConf = Config
  { pg = ConnectInfo
    { connectHost     = "localhost"
    , connectPort     = 15432
    , connectUser     = "postgres"
    , connectPassword = "postgres"
    , connectDatabase = "postgres"
    }
  }
