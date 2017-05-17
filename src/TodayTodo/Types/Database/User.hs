{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module for user creation, querying…
module TodayTodo.Types.Database.User where

import           Data.Int                     (Int64)
import           Data.Profunctor.Product.TH   (makeAdaptorAndInstance)
import           Data.Text                    (Text)
import           Data.Time.Clock

import           Opaleye (Column, Nullable, matchNullable, isNull,
                          Table(Table), required, queryTable,
                          Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                          (.===),
                          (.++), ifThenElse, pgString, aggregate, groupBy,
                          count, avg, sum, leftJoin, runQuery,
                          showSqlForPostgres, Unpackspec,
                          PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool,
                          PGTimestamptz)

data User' key user pass notbefore login disabled defaultimp autoraiseurg = User {
    uId                       :: key,
    uUsername                 :: user,
    uPassword                 :: pass,
    uNotBefore                :: notbefore,
    uLastLogin                :: login,
    uDisabled                 :: disabled,
    uTodayDefaultImportance   :: defaultimp,      -- when you add a new task in "Today", what should it's default importance be?
    uAutoRaiseUrgency         :: autoraiseurg     -- Do you want tasks to automatically rise in importance the closer they come to their due date,
                                                  -- according to what our patented artificial intelligence model thinks is best?
}

type User            = User' Int64 Text Text UTCTime (Maybe UTCTime) Bool Int Bool
type UserReadColumn  = User' (Column PGInt8)         (Column PGText) (Column PGText) (Column PGTimestamptz) (Column (Nullable PGTimestamptz)) (Column PGBool) (Column PGInt4) (Column PGBool)
type UserWriteColumn = User' (Maybe (Column PGInt8)) (Column PGText) (Column PGText) (Column PGTimestamptz) (Column (Nullable PGTimestamptz)) (Column PGBool) (Column PGInt4) (Column PGBool)

$(makeAdaptorAndInstance "pUser" ''User')
