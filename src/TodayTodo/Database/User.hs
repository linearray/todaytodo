{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module TodayTodo.Database.User where

import           Prelude hiding (sum,null)

import           Opaleye                          (Column, Nullable, matchNullable, isNull,
                                                   Table(Table), required, optional, queryTable,
                                                   Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                                                   (.===),
                                                   (.++), ifThenElse, pgString, aggregate, groupBy,
                                                   count, avg, sum, leftJoin, runQuery, runInsertMany, runUpdate,
                                                   showSqlForPostgres, Unpackspec,
                                                   null, toNullable,
                                                   PGInt4, pgInt4, PGInt8, pgInt8, PGText, pgStrictText, PGDate, PGFloat8,
                                                   PGBool, pgBool, PGTimestamptz, pgUTCTime)

import           Data.Int                         (Int64)
import           Data.Maybe                       (fromMaybe)
import           Data.Pool
import           Data.Profunctor.Product          (p2, p3)
import           Data.Profunctor.Product.Default  (Default)
import           Data.Profunctor.Product.TH       (makeAdaptorAndInstance)
import           Data.Text                        (Text)
import           Data.Time.Calendar               (Day)
import           Data.Time.Clock                  (getCurrentTime)
import           Database.PostgreSQL.Simple       (Connection)

import           Control.Arrow                    (returnA, (<<<))

import qualified Database.PostgreSQL.Simple    as PGS

import           TodayTodo.Types.Database.User

userTable :: Table UserWriteColumn UserReadColumn
userTable = Table "user" (pUser User { uId                     = optional "id"
                                     , uEmail                  = required "email"
                                     , uDisplayName            = required "display_name"
                                     , uPassword               = required "password"
                                     , uNotBefore              = required "not_before"
                                     , uLastLogin              = required "last_login"
                                     , uDisabled               = required "disabled"
                                     , uTodayDefaultImportance = required "today_default_importance"
                                     , uAutoRaiseUrgency       = required "auto_raise_urgency" })


userQuery :: Query UserReadColumn
userQuery = queryTable userTable

userByIdQuery :: Int64 -> Query UserReadColumn
userByIdQuery u = proc () -> do
    row@(User uid _ _ _ _ _ _ _) <- userQuery -< ()

    restrictToUserId u -< uid

    returnA -< row

userByEmailQuery :: Text -> Query UserReadColumn
userByEmailQuery u = proc () -> do
    row@(User _ name _ _ _ _ _ _) <- userQuery -< ()

    restrictToUserEmail u -< name

    returnA -< row

restrictToUserId :: Int64 -> QueryArr (Column PGInt8) ()
restrictToUserId u = proc user ->
    restrict -< user .== pgInt8 u

restrictToUserEmail :: Text -> QueryArr (Column PGText) ()
restrictToUserEmail u = proc user ->
    restrict -< user .== pgStrictText u


insertUser :: Connection -> User -> IO Int64
insertUser c User{..} = do
    t <- getCurrentTime
    runInsertMany c userTable [User Nothing (pgStrictText uUsername) (pgStrictText uPassword) (pgUTCTime t) (maybeToNull $ fmap (toNullable . pgUTCTime) uLastLogin) (pgBool uDisabled) (pgInt4 uTodayDefaultImportance) (pgBool uAutoRaiseUrgency)]

updateUser :: Connection -> User -> IO Int64
updateUser c User{..} =
    runUpdate c userTable
                (\case (User key email displayname password notbefore lastlogin disabled defaultimportance autoraise) ->
                        User Nothing (pgStrictText uEmail) (pgStrictText uUsername) (pgStrictText uPassword) (pgUTCTime uNotBefore) (maybeToNull $ fmap (toNullable . pgUTCTime) uLastLogin) (pgBool uDisabled) (pgInt4 uTodayDefaultImportance) (pgBool uAutoRaiseUrgency))
                (\case (User a _ _ _ _ _ _ _) -> a .== pgInt8 uId)

maybeToNull = fromMaybe null
