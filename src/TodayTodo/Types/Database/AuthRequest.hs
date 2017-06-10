{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module TodayTodo.Types.Database.AuthRequest where

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

data AuthRequest' a b c d e f g h i = AuthRequest {
    reqId          :: a,
    reqUserId      :: b,
    reqClientId    :: c,
    reqClientName  :: e,
    reqClientOs    :: f,
    reqCreated     :: g,
    reqLastAttempt :: h,
    reqAttempts    :: i
}

type AuthRequest            = AuthRequest' Int64 Int64 Text Text Text UTCTime UTCTime Int
type AuthRequestReadColumn  = AuthRequest'        (Column PGInt8)  (Column PGInt8) (Column PGText) (Column PGText) (Column PGText) (Column PGTimestamptz) (Column PGTimestamptz) (Column PGInt4)
type AuthRequestWriteColumn = AuthRequest' (Maybe (Column PGInt8)) (Column PGInt8) (Column PGText) (Column PGText) (Column PGText) (Column PGTimestamptz) (Column PGTimestamptz) (Column PGInt4)

$(makeAdaptorAndInstance "pAuthRequest" ''AuthRequest')
