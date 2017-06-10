{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}


module TodayTodo.Api.Todo where

import Control.Monad.IO.Class
import Data.Int
import Data.Pool
import Servant

import TodayTodo.Auth
import TodayTodo.Database.Todo
import TodayTodo.Types.Api.Todo
import TodayTodo.Types.ApiDefs.TodoApi
import TodayTodo.Types.Session
import TodayTodo.Types.Todo
import TodayTodo.Types.TodoServer

--handleTodoApi :: TodoServer -> SessionAuth -> Int64 -> Server TodoApi
handleTodoApi s (SessionAuth sess) i = get :<|> put
    where
        get   = liftIO $ withResource (sDbPool s) $ \conn -> fmap toTodoOut <$> getTodoForUser conn (sessUserId sess) i
        put t = liftIO $ withResource (sDbPool s) $ \conn ->
            updateTodoForUser conn (toTodo i (sessUserId sess) t) >>
                fmap toTodoOut <$> getTodoForUser conn (sessUserId sess) i

handleTodosApi :: TodoServer -> SessionAuth -> Server TodosApi
handleTodosApi s (SessionAuth sess) = get :<|> post
    where
        get    = liftIO $ withResource (sDbPool s) $ \conn -> fmap toTodoOut <$> getTodosForUser conn (sessUserId sess)
        post t = withResource (sDbPool s) $ \conn -> do
            i <- liftIO $ insertTodo conn (toTodo (-1) (sessUserId sess) t)
            t <- liftIO $ getTodoForUser conn (sessUserId sess) i
            maybe bailout
                  (return . toTodoOut)
                  t

bailout :: Handler TodoOut
bailout = throwError (err500 {errBody = "Just inserted record is not present"})

-- From the context of a request and the supplied JSON, generate a Todo record.
toTodo :: Int64 -> Int64 -> TodoIn -> Todo
toTodo i u TodoIn{..} = Todo {
    tId          = i,
    tUserId      = u,
    tName        = todoInName,
    tDescription = todoInDescription,
    tImportance  = todoInImportance,
    tUrgency     = todoInUrgency,
    tOpen        = todoInOpen,
    tDueDate     = todoInDueDate
}

toTodoOut :: Todo -> TodoOut
toTodoOut Todo {..} = TodoOut {
    todoOutId          = tId,
    todoOutName        = tName,
    todoOutDescription = tDescription,
    todoOutImportance  = tImportance,
    todoOutUrgency     = tUrgency,
    todoOutOpen        = tOpen,
    todoOutDueDate     = tDueDate
}
