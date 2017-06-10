{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}

module TodayTodo.Api where

import Data.Pool
import Data.String
import Data.Text.Lazy as Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import TodayTodo.Api.Todo        (handleTodoApi, handleTodosApi)
import TodayTodo.Auth
import TodayTodo.Database.Todo
import TodayTodo.Types.Api.Todo
import TodayTodo.Types.ApiDefs.TodoApi
import TodayTodo.Types.TodoServer

type AuthApi =
    Post '[JSON]

type RootApi =
    "todos"     :> AuthProtect "session" :> TodosApi  :<|>
    "todo"      :> AuthProtect "session" :> TodoApi   -- :<|>
    --"authorize" :> AuthApi

rootApi :: Proxy RootApi
rootApi = Proxy

handleRootApi :: TodoServer -> Server RootApi
handleRootApi s = handleTodosApi s  :<|> handleTodoApi s -- :<|> handleAuthApi s
  where
      handleAuthApi s = undefined

app1 :: TodoServer -> Application
app1 s = serveWithContext rootApi (genAuthServerContext s) (handleRootApi s)

startApp :: TodoServer -> IO ()
startApp s =
    let
        sHost    = fromString . Text.unpack $ (host . sConfig) s
        sPort    = (port . sConfig) s
        settings = setPort (fromInteger sPort) $ setHost sHost defaultSettings
    in runSettings settings (app1 s)
