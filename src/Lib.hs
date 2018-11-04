{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , User
    , UserApi
    ) where


import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Control.Monad.IO.Class

import Home (homePage)
import Users (User, id)
import qualified Db
import Routes

import Debug.Trace

-------------------------------------------------
-- Handle the routes

server :: Server Routes
server = home :<|> userApi :<|> assets
  where home = return homePage
        assets = serveDirectoryFileServer "frontend/dist"

userApi :: Server UserApi
userApi = getUsers :<|> postUsers :<|> deleteUser
  where
    getUsers :: Handler [User]
    getUsers = trace "get users" $ 
               liftIO $ Db.getUsers

    postUsers :: User -> Handler User
    postUsers user = trace "post user" $
      -- Return the user with the new id
      setId <$> (liftIO . Db.createUser) user
      where
        setId i = user { Users.id = Just i }

    deleteUser :: Int -> Handler String
    deleteUser id = trace "delete user" $
      -- Always return "Groovy" after deleting
      (const "Groovy") <$> 
      (liftIO $ Db.deleteUser id)


-----------------------------------------------
-- Set stuff up

routes :: Proxy Routes
routes = Proxy

app :: Application
app = serve routes server

someFunc :: IO ()
someFunc = do
  let port = 8000
  putStrLn $ "Running on " ++ (show port)
  run port app
