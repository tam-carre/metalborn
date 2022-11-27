{-# OPTIONS_GHC -Wno-unused-foralls #-}
{-# LANGUAGE AllowAmbiguousTypes, OverloadedRecordDot, ScopedTypeVariables #-}

module App (App (..), AppError (..), DBError (..), Env (..), intercept, mkEnv, runApp') where

import Config                     (Config (..), appConf)
import Control.Exception.Safe     (catch)
import Database.PostgreSQL.Simple qualified as PGS
import Relude.Extra.Newtype       (un)

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

mkEnv ∷ MonadIO m ⇒ m Env
mkEnv = liftIO (Env <$> PGS.connect appConf.pg)

runApp ∷ MonadIO m ⇒ Env → App a → m (Either AppError a)
runApp env = liftIO . runExceptT . usingReaderT env . un . catchAllSyncExcs where
  catchAllSyncExcs = (`catchAny` (throwError . UnknownError . toText . displayException))

runApp' ∷ MonadIO m ⇒ App a → m (Either AppError a)
runApp' = (mkEnv ≫=) . runningApp

runningApp ∷ MonadIO m ⇒ App a → Env → m (Either AppError a)
runningApp = flip runApp

intercept ∷ ∀ e a . Exception e ⇒ App a → (e → AppError) → App a
intercept app toAppErr = catch @App @e app (throwError . toAppErr)
