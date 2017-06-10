{-# LANGUAGE DeriveGeneric     #-}

module TodayTodo.Types.TodoServer where

import Data.Map
import Data.Pool
import Database.PostgreSQL.Simple
import Dhall
import GHC.Generics

data Config = Config {
    dbhost     :: Text,
    dbname     :: Text,
    dbuser     :: Text,
    dbpass     :: Text,
    host       :: Text,
    port       :: Integer,
    signsecret :: Text,
    signpublic :: Text
} deriving (Generic,Show)

instance Interpret Config

data TodoServer = TodoServer {
    sDbPool :: Pool Connection,
    sConfig :: Config
}
