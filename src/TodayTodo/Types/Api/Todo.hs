{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module TodayTodo.Types.Api.Todo where

import           Data.Aeson
import           Data.Int
import           Data.Text           (Text)
import           Data.Time.Clock


data TodoOut = TodoOut {
    todoOutId          :: Int64,
    todoOutName        :: Text,
    todoOutDescription :: Text,
    todoOutImportance  :: Int,
    todoOutUrgency     :: Int,
    todoOutOpen        :: Bool,
    todoOutDueDate     :: Maybe UTCTime
}

data TodoIn = TodoIn {
    todoInName        :: Text,
    todoInDescription :: Text,
    todoInImportance  :: Int,
    todoInUrgency     :: Int,
    todoInOpen        :: Bool,
    todoInDueDate     :: Maybe UTCTime
}

instance ToJSON TodoOut where
    toJSON TodoOut{..} = object [
        "id"          .= todoOutId,
        "name"        .= todoOutName,
        "description" .= todoOutDescription,
        "importance"  .= todoOutImportance,
        "urgency"     .= todoOutUrgency,
        "open"        .= todoOutOpen,
        "due_date"    .= todoOutDueDate
        ]

instance FromJSON TodoIn where
    parseJSON = withObject "TodoIn "$ \o -> do
        todoInName         <- o .:  "name"
        todoInDescription  <- o .:  "description"
        todoInImportance   <- o .:  "importance"
        todoInUrgency      <- o .:  "urgency"
        todoInOpen         <- o .:  "open"
        todoInDueDate      <- o .:? "due_date"

        return TodoIn {..}
