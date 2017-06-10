{-# LANGUAGE OverloadedStrings #-}

module TodayTodo.Database.Init where

import Data.ByteString
import Data.Monoid
import Data.Pool
import Database.PostgreSQL.Simple

import TodayTodo.Types.TodoServer

initDev :: TodoServer -> IO ()
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

dropTables = [ "DROP TABLE IF EXISTS \"user\"     CASCADE",
               "DROP TABLE IF EXISTS todo         CASCADE",
               "DROP TABLE IF EXISTS session      CASCADE",
               "DROP TABLE IF EXISTS auth_request CASCADE"
             ]

createTables = [
    "CREATE TABLE \"user\" ("                                                                      <>
    "    id                       SERIAL PRIMARY KEY,"                                             <>
    "    email                    TEXT        NOT NULL,"                                           <>
    "    display_name             TEXT        NOT NULL,"                                           <>
    "    password                 TEXT        NOT NULL,"                                           <>
    "    not_before               TIMESTAMPTZ NOT NULL,"                                           <>
    "    last_login               TIMESTAMPTZ     NULL,"                                           <>
    "    disabled                 BOOLEAN     NOT NULL,"                                           <>
    "    today_default_importance INTEGER         NULL,"                                           <>
    "    auto_raise_urgency       INTEGER         NULL,"                                           <>
    "    CONSTRAINT user_ukey UNIQUE (email)"                                                      <>
    ")",
    "CREATE TABLE todo ("                                                                          <>
    "    id            SERIAL PRIMARY KEY,"                                                        <>
    "    user_id       BIGINT REFERENCES \"user\" ON UPDATE CASCADE ON DELETE CASCADE,"            <>
    "    name          TEXT         NOT NULL,"                                                     <>
    "    description   TEXT         NOT NULL,"                                                     <>
    "    importance    INTEGER      NOT NULL,"                                                     <>
    "    urgency       INTEGER      NOT NULL,"                                                     <>
    "    done          BOOLEAN      NOT NULL,"                                                     <>
    "    due_date      TIMESTAMPTZ      NULL"                                                      <>
    ")",
    "CREATE TABLE session ("                                                                       <>
    "    id                       SERIAL PRIMARY KEY,"                                             <>      -- predictable, do not send over wire
    "    user_id                  BIGINT REFERENCES \"user\" ON UPDATE CASCADE ON DELETE CASCADE," <>
    "    secret                   TEXT        NOT NULL,"                                           <>      -- must be sent by clients for every request to protected resource
    "    client_id                TEXT            NULL,"                                           <>
    "    client_name              TEXT        NOT NULL,"                                           <>      -- A client's name, if present (like device name)
    "    client_os                TEXT        NOT NULL,"                                           <>      -- Client's Operating system or browser name
    "    created                  TIMESTAMPTZ NOT NULL,"                                           <>
    "    last_used                TIMESTAMPTZ NOT NULL,"                                           <>
    "    expires                  TIMESTAMPTZ NOT NULL,"                                           <>      -- Sessions have an expiration date set on creation.
    "    CONSTRAINT session_ukey UNIQUE (secret)"                                                  <>
    ")",
    "CREATE TABLE auth_request ("                                                                  <>
    "    id                       SERIAL PRIMARY KEY,"                                             <>      -- predictable, do not send over wire
    "    user_id                  BIGINT REFERENCES \"user\" ON UPDATE CASCADE ON DELETE CASCADE," <>
    "    request_id               TEXT        NOT NULL,"                                           <>      -- Non-predictable value, only used to acknowledge the request
    "    client_id                TEXT        NOT NULL,"                                           <>      -- Uniquely identifies a device, like iPhone UUID or similar, can also be random value from web browser
    "    client_name              TEXT        NOT NULL,"                                           <>      -- A client's name, if present (like device name)
    "    client_os                TEXT        NOT NULL,"                                           <>      -- Client's Operating system or browser name
    "    created                  TIMESTAMPTZ NOT NULL,"                                           <>      -- How long Requests are valid is specified in code, not data.
    "    last_attempt             TIMESTAMPTZ NOT NULL,"                                           <>
    "    attempts                 INTEGER     NOT NULL,"                                           <>      -- How often did we send an email for this request?
    "    CONSTRAINT request_urid UNIQUE (request_id),"                                             <>      -- request_ids must be unique
    "    CONSTRAINT request_ucid UNIQUE (client_id)"                                               <>      -- and per device there must be only one request pending
    ")"]
