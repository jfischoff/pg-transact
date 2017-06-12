{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, RecordWildCards, OverloadedStrings #-}
module Database.PostgreSQL.Transact where
import Control.Monad.Trans.Reader
import Database.PostgreSQL.Simple as Simple
import Database.PostgreSQL.Simple.Transaction
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Catch
import Data.Int
import qualified Data.ByteString as BS

newtype DBT m a = DBT { unDBT :: ReaderT Connection m a }
  deriving (MonadTrans, MonadThrow)

type DB = DBT IO

instance Functor m => Functor (DBT m) where
  fmap f = DBT . fmap f . unDBT

instance Applicative m => Applicative (DBT m) where
  pure = DBT . pure
  f <*> v = DBT $ unDBT f <*> unDBT v

instance MonadIO m => MonadIO (DBT m) where
  liftIO = lift . liftIO

instance Monad m => Monad (DBT m) where
  return = lift . return
  DBT m >>= k = DBT $ m >>= unDBT . k

isClass25 :: SqlError -> Bool
isClass25 SqlError{..} = BS.take 2 sqlState == "25"

instance (MonadIO m, MonadMask m) => MonadCatch (DBT m) where
  catch (DBT act) handler = DBT $ mask $ \restore -> do
    conn <- ask
    sp <- liftIO $ newSavepoint conn
    let setup = catch (restore act)
              $ \e -> case fromException e of
                        Nothing -> throwM e
                        Just x  -> do
                          liftIO $ rollbackToSavepoint conn sp
                          unDBT $ handler x

        cleanup = liftIO $ releaseSavepoint conn sp `catch` \e ->
                    if isClass25 e then
                      return () -- transaction error. We don't care.
                    else
                      throwM e

    setup `finally` cleanup

getConnection :: Monad m => DBT m Connection
getConnection = DBT ask

runDBT :: MonadBaseControl IO m => DBT m a -> IsolationLevel -> Connection -> m a
runDBT action level conn
  = control
  $ \run -> withTransactionLevel level conn
  $ run
  $ runReaderT (unDBT action) conn

runDBTSerializable :: MonadBaseControl IO m => DBT m a -> Connection -> m a
runDBTSerializable action conn
  = control
  $ \run -> withTransactionSerializable conn
  $ run
  $ runReaderT (unDBT action) conn

query :: (ToRow a, FromRow b, MonadIO m) => Query -> a -> DBT m [b]
query q x = getConnection >>= \conn -> liftIO $ Simple.query conn q x

query_ :: (FromRow b, MonadIO m) => Query -> DBT m [b]
query_ q = getConnection >>= \conn -> liftIO $ Simple.query_ conn q

execute :: (ToRow q, MonadIO m) => Query -> q -> DBT m Int64
execute q x = getConnection >>= \conn -> liftIO $ Simple.execute conn q x

execute_ :: MonadIO m => Query -> DBT m Int64
execute_ q = getConnection >>= \conn -> liftIO $ Simple.execute_ conn q

executeMany :: (ToRow q, MonadIO m) => Query -> [q] -> DBT m Int64
executeMany q xs = getConnection >>= \conn -> liftIO $ Simple.executeMany conn q xs

returning :: (ToRow q, FromRow r, MonadIO m) => Query -> [q] -> DBT m [r]
returning q xs = getConnection >>= \conn -> liftIO $ Simple.returning conn q xs