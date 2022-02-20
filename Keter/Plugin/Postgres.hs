{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Keter.Plugin.Postgres
    ( -- * Settings
      Settings
    , setupDBInfo
      -- * Functions
    , load
    ) where

import           Control.Applicative       ((<$>), (<*>), pure)
import           Data.Aeson.KeyHelper      as AK (lookup)
import           Control.Concurrent        (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception         (throwIO, try)
import           Control.Monad             (forever, mzero, replicateM, void)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import qualified Data.Char                 as C
import           Data.Default
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Builder    (fromText, toLazyText)
import qualified Data.Vector               as V
import           Data.Yaml
import           Keter.Types
import           Prelude                   hiding (FilePath)
import           System.Directory          (createDirectoryIfMissing,
                                            doesFileExist, renameFile)
import           System.FilePath           (takeDirectory, (<.>))
import           System.Process            (readProcess)
import qualified System.Random             as R

data Settings = Settings
    { setupDBInfo :: DBInfo -> IO ()
      -- ^ How to create the given user/database. Default: uses the @psql@
      -- command line tool and @sudo -u postgres@.
    }

instance Default Settings where
    def = Settings
        { setupDBInfo = \DBInfo{..} -> do
            let sql = toLazyText $
                    "CREATE USER "         <> fromText dbiUser <>
                    " PASSWORD '"          <> fromText dbiPass <>
                    "';\nCREATE DATABASE " <> fromText dbiName <>
                    " OWNER "              <> fromText dbiUser <>
                    ";"
                (cmd, args) 
                    | (  dbServer dbiServer == "localhost" 
                      || dbServer dbiServer == "127.0.0.1") = 
                        ("sudo", ["-u", "postgres", "psql"])
                    | otherwise = 
                        ("psql",
                        [ "-h", (T.unpack $ dbServer dbiServer)
                        , "-p", (show $ dbPort dbiServer)
                        , "-U", "postgres"])
            _ <- readProcess cmd args $ TL.unpack sql
            return ()
        }

-- | Information on an individual PostgreSQL database.
data DBInfo = DBInfo
    { dbiName   :: Text
    , dbiUser   :: Text
    , dbiPass   :: Text
    , dbiServer :: DBServerInfo
    }
    deriving Show

data DBServerInfo = DBServerInfo
    { dbServer :: Text
    , dbPort   :: Int
    }
    deriving Show

randomDBI :: DBServerInfo -> R.StdGen -> (DBInfo, R.StdGen)
randomDBI dbsi =
    S.runState (DBInfo <$> rt <*> rt <*> rt <*> (pure dbsi)) 
  where
    rt = T.pack <$> replicateM 10 (S.state $ R.randomR ('a', 'z'))

instance ToJSON DBInfo where
    toJSON (DBInfo n u p (DBServerInfo server port)) = object
        [ "name"   .= n
        , "user"   .= u
        , "pass"   .= p
        , "server" .= server
        , "port"   .= port
        ]

instance FromJSON DBInfo where
    parseJSON (Object o) = DBInfo
        <$> o .: "name"
        <*> o .: "user"
        <*> o .: "pass"
        <*> (DBServerInfo
            <$> o .:? "server" .!= "localhost"
            <*> o .:? "port"   .!= 5432)
    parseJSON _ = mzero

instance FromJSON DBServerInfo where
    parseJSON (Object o) = DBServerInfo
        <$> o .: "server"
        <*> o .: "port"
    parseJSON _ = mzero
    
instance Default DBServerInfo where
    def = DBServerInfo "localhost" 5432

data Command = GetConfig Appname DBServerInfo (Either SomeException DBInfo -> IO ())

-- | Load a set of existing connections from a config file. If the file does
-- not exist, it will be created. Any newly created databases will
-- automatically be saved to this file.
load :: Settings -> FilePath -> IO Plugin
load Settings{..} fp = do
    createDirectoryIfMissing True $ takeDirectory fp
    e <- doesFileExist fp
    edb <- if e
        then decodeFileEither fp
        else return $ Right Map.empty
    case edb of
        Left ex -> throwIO ex
        Right db -> go db
  where
    go db0 = do
        chan <- newChan
        g0 <- R.newStdGen
        -- FIXME stop using the worker thread approach?
        void $ forkIO $ flip S.evalStateT (db0, g0) $ forever $ loop chan
        return Plugin
            { pluginGetEnv = \appname o ->
                case AK.lookup "postgres" o of
                    Just (Array v) -> do
                        let dbServer = fromMaybe def . parseMaybe parseJSON $ V.head v
                        doenv chan appname dbServer
                    Just (Bool True) -> do
                        doenv chan appname def
                    _ -> return []
            }
      where doenv chan appname dbs = do
            x <- newEmptyMVar
            writeChan chan $ GetConfig appname dbs $ putMVar x
            edbi <- takeMVar x
            edbiToEnv edbi
                    
    tmpfp = fp <.> "tmp"

    loop chan = do
        GetConfig appname dbServer f <- lift $ readChan chan
        (db, g) <- S.get
        dbi <-
            case Map.lookup appname db of
                Just dbi -> return $ Right dbi
                Nothing -> do
                    let (dbi', g') = randomDBI dbServer g
                    let dbi = dbi'
                            { dbiName = sanitize appname <> dbiName dbi'
                            , dbiUser = sanitize appname <> dbiUser dbi'
                            }
                    ex <- lift $ try $ setupDBInfo dbi
                    case ex of
                        Left e -> return $ Left e
                        Right () -> do
                            let db' = Map.insert appname dbi db
                            ey <- lift $ try $ do
                                encodeFile tmpfp db'
                                renameFile tmpfp fp
                            case ey of
                                Left e -> return $ Left e
                                Right () -> do
                                    S.put (db', g')
                                    return $ Right dbi
        lift $ f dbi

    sanitize = T.map sanitize'
    sanitize' c
        | 'A' <= c && c <= 'Z' = C.toLower c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = '_'

edbiToEnv :: Either SomeException DBInfo
          -> IO [(Text, Text)]
edbiToEnv (Left e) = throwIO e
edbiToEnv (Right dbi) = return
    [ ("PGHOST", dbServer $ dbiServer dbi)
    , ("PGPORT", T.pack . show . dbPort $ dbiServer dbi)
    , ("PGUSER", dbiUser dbi)
    , ("PGPASS", dbiPass dbi)
    , ("PGDATABASE", dbiName dbi)
    ]

