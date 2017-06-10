-- for now just reexport the DB type

module TodayTodo.Types.Todo (
    Todo, Todo'(..)
) where

import TodayTodo.Types.Database.Todo (Todo, Todo'(..))
