{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module TodayTodo.Types.Database.Session where

import           Data.Int                     (Int64)
import           Data.Profunctor.Product.TH   (makeAdaptorAndInstance)
import           Data.Text                    (Text)
import           Data.Time.Clock

import           Opaleye                      (Column, Nullable, matchNullable, isNull,
                                               Table(Table), required, queryTable,
                                               Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                                               (.===),
                                               (.++), ifThenElse, pgString, aggregate, groupBy,
                                               count, avg, sum, leftJoin, runQuery,
                                               showSqlForPostgres, Unpackspec,
                                               PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool,
                                               PGTimestamptz)

data Session' a b c d e f g h i = Session {
    sessId          :: a,
    sessUserId      :: b,
    sessSecret      :: c,
    sessClientId    :: d,
    sessClientName  :: e,
    sessClientOs    :: f,
    sessCreated     :: g,
    sessLastUsed    :: h,
    sessExpires     :: i
}

type Session            = Session' Int64 Int64 Text Text Text Text UTCTime UTCTime UTCTime
type SessionReadColumn  = Session'        (Column PGInt8)  (Column PGInt8) (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGTimestamptz) (Column PGTimestamptz) (Column PGTimestamptz)
type SessionWriteColumn = Session' (Maybe (Column PGInt8)) (Column PGInt8) (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGTimestamptz) (Column PGTimestamptz) (Column PGTimestamptz)

$(makeAdaptorAndInstance "pSession" ''Session')
