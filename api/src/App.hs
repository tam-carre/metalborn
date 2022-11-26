{-# LANGUAGE OverloadedRecordDot, ScopedTypeVariables #-}

module App (App (..), AppError (..), DBError (..), Env (..), intercept, mkEnv, runApp) where

import Config                     (Config (..), appConf)
import Control.Exception.Safe     (catch)
import Database.PostgreSQL.Simple qualified as PGS

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

mkEnv ∷ MonadIO m ⇒ m Env
mkEnv = Env <$> liftIO (PGS.connect appConf.pg)

data AppError
  = DBError DBError
  | UnknownError Text
  deriving (Eq, Generic, Show)

data DBError
  = UniqueConstraintViolated
  | OtherDBError PGS.SqlError
  deriving (Eq, Generic, Show)

runAppWith ∷ MonadIO m ⇒ App a → Env → m (Either AppError a)
runAppWith app env = liftIO
                   . runExceptT
                   . usingReaderT env
                   . unApp
                   . safeize
                   $ app

runApp ∷ MonadIO m ⇒ App a → m (Either AppError a)
runApp app = runAppWith app =≪ mkEnv

unApp ∷ App a → ReaderT Env (ExceptT AppError IO) a
unApp (App a) = a

intercept ∷ ∀ e a . Exception e ⇒ (e → AppError) → App a → App a
intercept toAppErr app = catch @App @e app (throwError . toAppErr)

-- Make all runtime exceptions be caught into UnknownError
safeize ∷ App a → App a
safeize = (`catchAny` (throwError . UnknownError . toText . displayException))
