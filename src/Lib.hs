{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    ( someFunc
    , User
    , UserApi
    ) where

import Prelude ()
import Prelude.Compat

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Control.Monad.IO.Class

import Home (homePage)
import Users (User)
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
userApi = getUsers :<|> postUsers
  where
    getUsers = trace "get users" $ do
      users <- liftIO $ Db.getUsers
      return users

    postUsers user = trace "post user" $ do
      liftIO $ Db.createUser user
      return user


-----------------------------------------------

routes :: Proxy Routes
routes = Proxy

app :: Application
app = serve routes server

someFunc :: IO ()
someFunc = do
  let port = 8000
  putStrLn $ "Running on " ++ (show port)
  run port app
