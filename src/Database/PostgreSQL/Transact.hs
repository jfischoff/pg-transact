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
import Control.Monad
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
    sp   <- liftIO $ newSavepoint conn
    let setup = catch (restore act) $ \e -> do
                  liftIO $ rollbackToSavepoint conn sp
                  unDBT $ handler e

    setup `finally` liftIO (tryJust (guard . isClass25) (releaseSavepoint conn sp))

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

-- | Perform a @SELECT@ or other SQL query that is expected to return
-- results. All results are retrieved and converted before this
-- function returns.
--
-- When processing large results, this function will consume a lot of
-- client-side memory.  Consider using 'fold' instead.
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string could not be formatted correctly.
--
-- * 'QueryError': the result contains no columns (i.e. you should be
--   using 'execute' instead of 'query').
--
-- * 'ResultError': result conversion failed.
--
-- * 'SqlError':  the postgresql backend returned an error,  e.g.
--   a syntax or type error,  or an incorrect table or column name.
query :: (ToRow a, FromRow b, MonadIO m) => Query -> a -> DBT m [b]
query q x = getConnection >>= \conn -> liftIO $ Simple.query conn q x

-- | A version of 'query' that does not perform query substitution.
query_ :: (FromRow b, MonadIO m) => Query -> DBT m [b]
query_ q = getConnection >>= \conn -> liftIO $ Simple.query_ conn q

-- | Execute an @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Returns the number of rows affected.
--
-- Throws 'FormatError' if the query could not be formatted correctly, or
-- a 'SqlError' exception if the backend returns an error.
execute :: (ToRow q, MonadIO m) => Query -> q -> DBT m Int64
execute q x = getConnection >>= \conn -> liftIO $ Simple.execute conn q x

-- | A version of execute that does not perform query substitution.
execute_ :: MonadIO m => Query -> DBT m Int64
execute_ q = getConnection >>= \conn -> liftIO $ Simple.execute_ conn q

-- | Execute a multi-row @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Returns the number of rows affected.   If the list of parameters is empty,
-- this function will simply return 0 without issuing the query to the backend.
-- If this is not desired, consider using the 'Values' constructor instead.
--
-- Throws 'FormatError' if the query could not be formatted correctly, or
-- a 'SqlError' exception if the backend returns an error.
--
-- For example,  here's a command that inserts two rows into a table
-- with two columns:
--
-- @
-- executeMany [sql|
--     INSERT INTO sometable VALUES (?,?)
--  |] [(1, \"hello\"),(2, \"world\")]
-- @
--
-- Here's an canonical example of a multi-row update command:
--
-- @
-- executeMany [sql|
--     UPDATE sometable
--        SET sometable.y = upd.y
--       FROM (VALUES (?,?)) as upd(x,y)
--      WHERE sometable.x = upd.x
--  |] [(1, \"hello\"),(2, \"world\")]
-- @

executeMany :: (ToRow q, MonadIO m) => Query -> [q] -> DBT m Int64
executeMany q xs = getConnection >>= \conn -> liftIO $ Simple.executeMany conn q xs

-- | Execute @INSERT ... RETURNING@, @UPDATE ... RETURNING@, or other SQL
-- query that accepts multi-row input and is expected to return results.
-- Note that it is possible to write
--    @'query' conn "INSERT ... RETURNING ..." ...@
-- in cases where you are only inserting a single row,  and do not need
-- functionality analogous to 'executeMany'.
--
-- If the list of parameters is empty,  this function will simply return @[]@
-- without issuing the query to the backend.   If this is not desired,
-- consider using the 'Values' constructor instead.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
returning :: (ToRow q, FromRow r, MonadIO m) => Query -> [q] -> DBT m [r]
returning q xs = getConnection >>= \conn -> liftIO $ Simple.returning conn q xs