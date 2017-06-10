module TodayTodo.Types.AuthRequest where

import Data.Int
import Data.Text
import Data.Time.Clock

data AuthRequest = AuthRequest {
    reqId          :: Int64,
    reqUserId      :: Int64,
    reqClientId    :: Text,
    reqClientName  :: Text,
    reqClientOs    :: Text,
    reqCreated     :: UTCTime,
    reqLastAttempt :: UTCTime,
    reqAttempts    :: Int
}
