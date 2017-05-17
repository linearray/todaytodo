{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module TodayTodo.Types.Database.Todo where

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

data Todo' a b c d e f g h = Todo {
    tId          :: a,
    tUserId      :: b,
    tName        :: c,
    tDescription :: d,
    tImportance  :: e,
    tUrgency     :: f,
    tOpen        :: g,
    tDueDate     :: h
}

type Todo            = Todo' Int64 Int64 Text Text Int Int Bool (Maybe UTCTime)
type TodoReadColumn  = Todo'        (Column PGInt8)  (Column PGInt8) (Column PGText) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGBool) (Column (Nullable PGTimestamptz))
type TodoWriteColumn = Todo' (Maybe (Column PGInt8)) (Column PGInt8) (Column PGText) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGBool) (Column (Nullable PGTimestamptz))

$(makeAdaptorAndInstance "pTodo" ''Todo')
