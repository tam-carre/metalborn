{-# OPTIONS_GHC -Wno-unused-foralls #-}
{-# LANGUAGE OverloadedRecordDot #-}

module App
  ( App (..)
  , AppError (..)
  , DBError (..)
  , Env (..)
  , intercept
  , mkEnv
  , runAppWith
  , runServer
  ) where

import Config                     (Config (..), appConf)
import Control.Exception.Safe     (catch)
import Database.PostgreSQL.Simple qualified as PGS
import Network.Wai.Handler.Warp   (Port, run)
import Relude.Extra.Newtype       (un)
import Servant                    (Handler, HasServer, ServerT, err400, err500, hoistServer, serve)

----------------------------------------------------------------------------------------------------

newtype App a
  = App (ReaderT Env (ExceptT AppError IO) a)
  deriving
  ( Applicative
  , Functor
  , Monad
  , MonadCatch
  , MonadError AppError
  , MonadIO
  , MonadReader Env
  , MonadThrow
  )

newtype Env
  = Env { db ∷ PGS.Connection }
  deriving (Generic)

data AppError
  = DBError DBError
  | BadRequestError
  | UnknownError Text
  deriving (Eq, Generic, Show)

data DBError
  = UniqueConstraintViolated
  | OtherDBError PGS.SqlError
  deriving (Eq, Generic, Show)

runServer ∷ ∀ k (api ∷ k) . HasServer k '[] ⇒ Port → Proxy k → ServerT k App → IO ()
runServer port api server = do
  env ← mkEnv

  run port . serve api . hoistServer api (appToServantHandler env) $ server
  where
  appToServantHandler ∷ ∀ a . Env → App a → Handler a
  appToServantHandler env = runAppWith env ↣ \case
    Right a              → pure a
    Left BadRequestError → throwError err400
    Left e               → print e ≫ throwError err500

mkEnv ∷ MonadIO m ⇒ m Env
mkEnv = liftIO (Env <$> PGS.connect appConf.pg)

runAppWith ∷ MonadIO m ⇒ Env → App a → m (Either AppError a)
runAppWith env = liftIO . runExceptT . usingReaderT env . un . catchAllSyncExcs where
  catchAllSyncExcs = (`catchAny` (throwError . UnknownError . toText . displayException))

intercept ∷ ∀ e a . Exception e ⇒ App a → (e → AppError) → App a
intercept app toAppErr = catch @App @e app (throwError . toAppErr)
