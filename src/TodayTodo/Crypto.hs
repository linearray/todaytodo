module TodayTodo.SessionCrypto where

import Data.Int
import Data.Serialize
import Data.Text

newtype SessionData = SessionData {
    sessSecret :: Text
}
