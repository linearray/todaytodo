module TodayTodo.Types.Session (
    module X
) where

import Data.Int
import Data.Text
import Data.Time.Clock

import TodayTodo.Types.Database.Session as X

-- data Session = Session {
--     sessId         :: Int64,
--     sessUserId     :: Int64,
--     sessSecret     :: Text,
--     sessClientId   :: Text,
--     sessClientName :: Text,
--     sessClientOs   :: Text,
--     sessCreated    :: UTCTime,
--     sessLastUsed   :: UTCTime,
--     sessExpires    :: UTCTime
-- }
