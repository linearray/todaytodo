{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module TodayTodo.Database.Todo where

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
import           Data.Profunctor.Product         (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Data.Time.Calendar              (Day)
import           Database.PostgreSQL.Simple      (Connection)

import           Control.Arrow                   (returnA, (<<<))

import qualified Database.PostgreSQL.Simple   as PGS

import           TodayTodo.Types.Database.Todo

todoTable = Table "todo" (pTodo Todo { tId          = optional "id"
                                     , tUserId      = required "user_id"
                                     , tName        = required "name"
                                     , tDescription = required "description"
                                     , tImportance  = required "importance"
                                     , tUrgency     = required "urgency"
                                     , tOpen        = required "open"
                                     , tDueDate     = required "due_date" })


todoQuery :: Query TodoReadColumn
todoQuery = queryTable todoTable

todosForUser :: Int64 -> Query TodoReadColumn
todosForUser u = proc () -> do
    row@(Todo _ uid _ _ _ _ _ _) <- todoQuery -< ()

    restrictToUserId u -< uid

    returnA -< row

openTodosForUser :: Int64 -> Query TodoReadColumn
openTodosForUser u = proc () -> do
    row@(Todo _ _ _ _ _ _ o _) <- todosForUser u -< ()

    restrictToOpen -< o

    returnA -< row

restrictToUserId :: Int64 -> QueryArr (Column PGInt8) ()
restrictToUserId u = proc user ->
    restrict -< user .== pgInt8 u

restrictToOpen :: QueryArr (Column PGBool) ()
restrictToOpen = proc open ->
    restrict -< open .== pgBool True

insertTodo :: Connection -> Todo -> IO Int64
insertTodo c Todo{..} = runInsertMany c todoTable [Todo Nothing (pgInt8 tUserId) (pgStrictText tName) (pgStrictText tDescription) (pgInt4 tImportance) (pgInt4 tUrgency) (pgBool True) (maybeToNullable (fmap pgUTCTime tDueDate))]

updateTodo :: Connection -> Todo -> IO Int64
updateTodo c Todo{..} = runUpdate c todoTable
                                    (\case (Todo _ b _ _ _ _ _ _) -> Todo Nothing b (pgStrictText tName) (pgStrictText tDescription) (pgInt4 tImportance) (pgInt4 tUrgency) (pgBool tOpen) (maybeToNullable (fmap pgUTCTime tDueDate)))
                                    (\case (Todo a _ _ _ _ _ _ _) -> a .== pgInt8 tId)
