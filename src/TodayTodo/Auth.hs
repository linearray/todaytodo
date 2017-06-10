{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}

module TodayTodo.Auth (
    genAuthServerContext,
    SessionAuth(..)
) where

import           Control.Monad.IO.Class                     (liftIO)
import           Crypto.Saltine.Class
import           Crypto.Saltine.Core.Sign
import           Data.ByteString                            (ByteString)
import           Data.Pool
import qualified Data.Text                                as Text
import           Data.Text                                  (Text)
import           Data.Text.Encoding                         (encodeUtf8, decodeUtf8)
import           Network.Wai                                (Request, requestHeaders, rawPathInfo)
import           Servant
import           Servant.Server.Experimental.Auth
import           Web.Cookie

import qualified TodayTodo.Database.Session              as DB
import           TodayTodo.Types.Session
import           TodayTodo.Types.TodoServer

newtype SessionAuth = SessionAuth { aSess :: Session }

-- | 1) check whether the client sent a session cookie and whether the session secret is valid.
--   2a) If yes, attach the session to the request and be done
--   2b) Otherwise, deny.
verifySession :: TodoServer -> ByteString -> Handler SessionAuth
verifySession s sId =
    withResource (sDbPool s) $ \dbConn -> do
        let ss = decodeUtf8 sId
        dbs <- liftIO $ DB.getSessionForSecret dbConn ss
        maybe bailout
              (return . SessionAuth)
              dbs


-- | The auth handler wraps a function from Request -> Handler Account
--   we look for a cookie and pass its value to `lookupAccount`.
sessionAuthHandler :: TodoServer -> AuthHandler Request SessionAuth
sessionAuthHandler s =
  let handler req = case lookup "Cookie" (requestHeaders req) of
        Nothing -> bailout
        Just cs ->
            let
                coks = parseCookies cs
                sess = lookup "session" coks
            in
                maybe bailout (verifySession s) sess
  in mkAuthHandler handler

bailout = throwError (err403 {errBody = "Invalid/missing session"})


type instance AuthServerData (AuthProtect "session")               = SessionAuth

-- | The context that will be made available to request handlers. We supply the
-- "sessionauth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: TodoServer -> Context '[AuthHandler Request SessionAuth]
genAuthServerContext s = sessionAuthHandler s :. EmptyContext
