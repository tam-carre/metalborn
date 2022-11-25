{-# LANGUAGE OverloadedRecordDot #-}

module App (App (..), AppError (..), Env (..), runApp, withDb) where

import Control.Monad.Error.Class  (MonadError (..))
import Database.PostgreSQL.Simple qualified as PGS

----------------------------------------------------------------------------------------------------

newtype App a
  = App (ReaderT Env (ExceptT AppError IO) a)
  deriving (Applicative, Functor, Monad, MonadError AppError, MonadIO, MonadReader Env)

newtype Env
  = Env { db ∷ PGS.Connection }
  deriving (Generic)

data AppError
  = AppErrorDb Text
  | AppErrorOther Text

runApp ∷ App a → ReaderT Env (ExceptT AppError IO) a
runApp (App a) = a

withDb ∷ (PGS.Connection → a → IO b) → a → App b
withDb f x = do
  db ← asks (.db)
  liftIO $ f db x
