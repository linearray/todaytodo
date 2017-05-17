{-# LANGUAGE DeriveGeneric     #-}

module TodayTodo.Types.Server where

import Data.Map
import Data.Pool
import Database.PostgreSQL.Simple
import Dhall
import GHC.Generics

data Config = Config {
    dbhost :: Text,
    dbname :: Text,
    dbuser :: Text,
    dbpass :: Text
} deriving (Generic,Show)

instance Interpret Config

data Server = Server {
    sDbPool :: Pool Connection,
    sConfig :: Config
}
