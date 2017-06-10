{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module TodayTodo.Database.Session (
    getSessionForSecret,
    insertSession,
    updateSession
) where

import           Prelude hiding (sum)

import           Opaleye                         (Column, Nullable, matchNullable, isNull,
                                                  Table(Table), required, optional, queryTable,
                                                  Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                                                  (.===),
                                                  (.++), ifThenElse, pgString, aggregate, groupBy,
                                                  count, avg, sum, leftJoin, runQuery, maybeToNullable,
                                                  showSqlForPostgres, Unpackspec, runInsertMany, runUpdate,
                                                  PGInt4, pgInt4, PGInt8, pgInt8, PGText, pgStrictText, PGDate, pgUTCTime,
                                                  PGFloat8, PGBool, pgBool)

import           Data.Int                        (Int64)
import           Data.Maybe                      (listToMaybe)
import           Data.Profunctor.Product         (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Data.Text                       (Text)
import           Data.Time.Calendar              (Day)
import           Database.PostgreSQL.Simple      (Connection)

import           Control.Arrow                   (returnA, (<<<))

import qualified Database.PostgreSQL.Simple   as PGS

import           TodayTodo.Types.Database.Session


sessionTable = Table "session" (pSession Session { sessId          = optional "id"
                                                 , sessUserId      = required "user_id"
                                                 , sessSecret      = required "secret"
                                                 , sessClientId    = required "client_id"
                                                 , sessClientName  = required "client_name"
                                                 , sessClientOs    = required "client_os"
                                                 , sessCreated     = required "created"
                                                 , sessLastUsed    = required "last_used"
                                                 , sessExpires     = required "expires" })

sessionQuery :: Query SessionReadColumn
sessionQuery = queryTable sessionTable

sessionById :: Int64 -> Query SessionReadColumn
sessionById i = proc () -> do
    row@(Session sid _ _ _ _ _ _ _ _) <- sessionQuery -< ()

    restrictToId i -< sid

    returnA -< row


sessionForSecret :: Text -> Query SessionReadColumn
sessionForSecret s = proc () -> do
    row@(Session _ _ secret _ _ _ _ _ _) <- sessionQuery -< ()

    restrictToSecret s -< secret

    returnA -< row

restrictToSecret :: Text -> QueryArr (Column PGText) ()
restrictToSecret s = proc secret ->
    restrict -< secret .== pgStrictText s

restrictToId :: Int64 -> QueryArr (Column PGInt8) ()
restrictToId u = proc uid ->
    restrict -< uid .== pgInt8 u


getSessionForSecret :: Connection -> Text -> IO (Maybe Session)
getSessionForSecret c s = listToMaybe <$> runQuery c (sessionForSecret s)

insertSession :: Connection -> Session -> IO Int64
insertSession c Session{..} = runInsertMany c sessionTable [Session Nothing (pgInt8 sessUserId) (pgStrictText sessSecret) (pgStrictText sessClientId) (pgStrictText sessClientName) (pgStrictText sessClientOs) (pgUTCTime sessCreated) (pgUTCTime sessLastUsed) (pgUTCTime sessExpires)]

-- FIXME: Maybe this should only allow changes to sessLastUsed
updateSession :: Connection -> Session -> IO Int64
updateSession c Session{..} = runUpdate c sessionTable
                                    (\case Session{}                   -> Session Nothing (pgInt8 sessUserId) (pgStrictText sessSecret) (pgStrictText sessClientId) (pgStrictText sessClientName) (pgStrictText sessClientOs) (pgUTCTime sessCreated) (pgUTCTime sessLastUsed) (pgUTCTime sessExpires))
                                    (\case (Session a _ _ _ _ _ _ _ _) -> a .== pgInt8 sessId)
