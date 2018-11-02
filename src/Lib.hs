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
    , UserAPI
    ) where

import Prelude ()
import Prelude.Compat

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Home (Home, homePage)
import Users (User, UserAPI, users)

type Routes = Home
              :<|> "api" :> UserAPI 
              :<|> "assets" :> Raw

server :: Server Routes
server = home :<|> userApi :<|> assets
  where home = return homePage
        userApi = return users
        assets = serveDirectoryFileServer "frontend/dist"

routes :: Proxy Routes
routes = Proxy

app :: Application
app = serve routes server

someFunc :: IO ()
someFunc = do
  let port = 8000
  putStrLn $ "Running on " ++ (show port)
  run port app
