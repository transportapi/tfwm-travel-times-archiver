{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Concurrent (forkIO)

import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as A
import           Data.Maybe (fromMaybe)

import qualified Database.LevelDB.Base as LDB

import qualified API

import           GHC.Generics (Generic)

import           Options.Generic (ParseRecord, Wrapped, unwrapRecord, type (<?>), type (:::))

data Config w = Config
  { config   :: w ::: FilePath       <?> "Configuration file path"
  , database :: w ::: Maybe FilePath <?> "LevelDB path, default: ./travel_times"
  , port     :: w ::: Maybe Int      <?> "Port, default: 80"
  } deriving (Generic)

instance ParseRecord (Config Wrapped)

app :: API.Config -> FilePath -> Int -> IO ()
app cfg dbPath p = do
  ldb <- LDB.open dbPath LDB.defaultOptions
    { LDB.createIfMissing = True
    }

  _ <- forkIO $ API.scrape ldb cfg
  API.travelTimesServer ldb p

replay :: FilePath -> Int -> IO ()
replay dbPath p = do
  ldb <- LDB.open dbPath LDB.defaultOptions
    { LDB.createIfMissing = True
    }

  _ <- forkIO $ API.replay ldb
  API.travelTimesServer ldb p

serve :: FilePath -> Int -> IO ()
serve dbPath p = do
  ldb <- LDB.open dbPath LDB.defaultOptions
    { LDB.createIfMissing = True
    }
  API.travelTimesServer ldb p

main :: IO ()
main = do
  appCfg  <- unwrapRecord "TFWM Travel Times Archiver"
  cfgFile <- B.readFile (config appCfg)
  case A.eitherDecode cfgFile of
    Right cfg -> app
      cfg
      (fromMaybe "travel_times" $ database appCfg)
      (fromMaybe 80 $ port appCfg)
    Left e    -> error e
