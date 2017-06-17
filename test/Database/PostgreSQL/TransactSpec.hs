{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Database.PostgreSQL.TransactSpec where

import           Control.Monad              (void)
import           Control.Monad.Catch
import qualified Data.ByteString.Char8      as BSC
import           Data.String
import           Data.Typeable
import qualified Database.PostgreSQL.Simple as PS
import           Database.PostgreSQL.Simple ( Connection
                                            , Only (..)
                                            , SqlError (..)
                                            )
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Transact
import qualified Database.Postgres.Temp as Temp
import           Test.Hspec

-------------------------       Test DB Creation       -------------------------
createDB :: IO (Connection, Temp.DB)
createDB = do
    Right tempDB <- Temp.startAndLogToTmp []
    let connectionString = Temp.connectionString tempDB
    connection <- PS.connectPostgreSQL $ BSC.pack connectionString
    void $ PS.execute_ connection $
        [sql| CREATE TABLE fruit (name VARCHAR(100) PRIMARY KEY ) |]
    return (connection, tempDB)

shutdown :: (Connection, Temp.DB) -> IO ()
shutdown (conn, db) = do
  PS.close conn
  void $ Temp.stop db

-------------------------        Test Utilities        -------------------------
insertFruit :: String -> DB ()
insertFruit fruit
  = void $ execute [sql| INSERT INTO fruit (name) VALUES (?) |] (Only fruit)

fruits :: Connection -> IO [String]
fruits conn
  = fmap (map fromOnly)
  $ PS.query_ conn [sql|SELECT name FROM fruit ORDER BY name|]

runDB :: Connection -> DB a -> IO a
runDB = flip runDBTSerializable

shouldBeM :: (Eq a, Show a) => IO a -> a -> IO ()
shouldBeM action expected = do
    actual <- action
    actual `shouldBe` expected

-- Simple exception type for testing
data Forbidden = Forbidden
    deriving (Show, Eq, Typeable)

instance Exception Forbidden

-------------------------         Tests Start          -------------------------
spec :: Spec
spec = describe "TransactionSpec" $ do
    -- Notice the 'beforeAll'. The second test uses the same db as the first
    beforeAll createDB $ afterAll shutdown $ do
        it "execute_ happen path succeeds" $ \(conn, _) -> do
            let apple = "apple"
            runDB conn $ insertFruit apple

            fruits conn `shouldBeM` ["apple"]

        it "execute_ rollbacks on exception" $ \(conn, _) -> do
            flip shouldThrow (\(SqlError {}) -> True) $
                runDB conn $ do
                    insertFruit "orange"
                    -- This should cause an exception because of the UNIQUE
                    -- constraint on 'name'
                    insertFruit "apple"

            fruits conn `shouldBeM` ["apple"]

    before createDB $ do
        it "multiple execute_'s succeed" $ \(conn, _) -> do
            runDB conn $ do
                insertFruit "grapes"
                insertFruit "orange"

            fruits conn `shouldBeM` ["grapes", "orange"]

        it "throwM causes a rollback" $ \(conn, _) -> do
            flip shouldThrow (\Forbidden -> True) $
                runDB conn $ do
                    insertFruit "salak"
                    () <- throwM Forbidden
                    insertFruit "banana"

            fruits conn `shouldBeM` []

        it "query recovers when exception is caught" $ \(conn, _) -> do
            runDB conn $ do
                -- This should always happen because of the handle below
                insertFruit "banana"
                handle (\Forbidden -> insertFruit "tomato") $ do
                    insertFruit "salak"
                    throwM Forbidden

            fruits conn `shouldBeM` ["banana", "tomato"]

        it "multiple catch statements work correctly" $ \(conn, _) -> do
            runDB conn $ do
                insertFruit "banana"
                handle (\Forbidden -> insertFruit "tomato") $ do
                    -- This will happen ... even if there is an exception below
                    -- if we catch it
                    insertFruit "blueberry"
                    handle (\Forbidden -> insertFruit "frankenberry") $ do
                        insertFruit "salak"
                        throwM Forbidden

            fruits conn `shouldBeM` ["banana", "blueberry", "frankenberry"]

        it "alternate branches can also have savepoints" $ \(conn, _) -> do
            runDB conn $ do
                insertFruit "banana"
                catch (insertFruit "tomato" >> throwM Forbidden) $
                    \Forbidden -> do
                        insertFruit "blueberry"
                        handle (\Forbidden -> insertFruit "frankenberry") $ do
                            insertFruit "salak"
                            throwM Forbidden

            fruits conn `shouldBeM` ["banana", "blueberry", "frankenberry"]

        it "releasing silently fails if the transaction errors" $ \(conn, _) -> do
            runDB conn $ do
                insertFruit "banana"
                catchAll (void $ execute_ [sql| ABORT |]) $
                    \_ -> insertFruit "tomato"

            fruits conn `shouldBeM` []