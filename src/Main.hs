{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.ByteString.Lazy       (toStrict)
import qualified Data.Text.Lazy          as DTL
import qualified Data.Text.Lazy.Encoding as DTLE
import           Dhall

import           TodayTodo.Database.Init
import           TodayTodo.Types.Server

main :: IO ()
main = do
    (c :: Config) <- input auto "./todaytodo.config"

    let connStr = DTL.concat ["postgresql://",
                              dbuser c,
                              ":",
                              dbpass c,
                              "@",
                              dbhost c,
                              "/",
                              dbname c]

    pool <- createPool (toStrict $ DTLE.encodeUtf8 connStr) 20 900

    let s    = Server pool c

    print c
