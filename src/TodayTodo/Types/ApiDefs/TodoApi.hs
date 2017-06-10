{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}

module TodayTodo.Types.ApiDefs.TodoApi where

import Data.Int
import Data.Text
import Servant

import TodayTodo.Types.Api.Todo



-- /todo/$id
type TodoApi =
    Capture "id" Int64 :>
    (
                                 Get '[JSON] (Maybe TodoOut) :<|>
       ReqBody '[JSON] TodoIn :> Put '[JSON] (Maybe TodoOut)
    )

-- /todos
type TodosApi =
                              Get '[JSON] [TodoOut] :<|>
    ReqBody '[JSON] TodoIn :> Post '[JSON] TodoOut
