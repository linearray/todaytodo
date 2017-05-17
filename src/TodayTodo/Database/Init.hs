{-# LANGUAGE OverloadedStrings #-}

module TodayTodo.Database.Init where

import Data.ByteString
import Data.Pool
import Database.PostgreSQL.Simple

import TodayTodo.Types.Server

initDev :: Server -> IO ()
initDev s = withResource (sDbPool s) $ \c -> do
    mapM_ (execute_ c) dropTables
    mapM_ (execute_ c) createTables

openConnection :: ByteString -> IO Connection
openConnection = connectPostgreSQL

closeConnection :: Connection -> IO ()
closeConnection = close

createPool :: ByteString -> Int -> Int -> IO (Pool Connection)
createPool connStr maxConnections idleTimeout =
    Data.Pool.createPool (openConnection connStr) closeConnection 1 (realToFrac idleTimeout) maxConnections

dropTables = [ "DROP TABLE todo CASCADE",
               "DROP TABLE user CASCADE"
             ]

createTables = [
    "CREATE TABLE todo (                                                               \
    \    id            SERIAL PRIMARY KEY,                                             \
    \    user_id       BIGINT REFERENCES \"user\" ON UPDATE CASCADE ON DELETE CASCADE, \
    \    name          TEXT         NOT NULL,                                          \
    \    description   TEXT         NOT NULL,                                          \
    \    importance    INTEGER      NOT NULL,                                          \
    \    urgency       INTEGER      NOT NULL,                                          \
    \    done          BOOLEAN      NOT NULL,                                          \
    \    due_date      TIMESTAMPTZ      NULL                                           \
    \)",
    "CREATE TABLE \"user\" (                                                           \
    \    id                       SERIAL PRIMARY KEY,                                  \
    \    username                 TEXT        NOT NULL,                                \
    \    password                 TEXT        NOT NULL,                                \
    \    not_before               TIMESTAMPTZ NOT NULL,                                \
    \    last_login               TIMESTAMPTZ     NULL,                                \
    \    disabled                 BOOLEAN     NOT NULL,                                \
    \    today_default_importance INTEGER         NULL,                                \
    \    auto_raise_urgency       INTEGER         NULL                                 \
    \)" ]
