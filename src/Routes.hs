{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | 

module Routes where

import Servant
import Lucid (Html)
import Servant.HTML.Lucid (HTML)
import Users (User)

type Home = Get '[HTML] (Html ())

type UserApi = "users" :> Get '[JSON] [User]
               :<|> ReqBody '[JSON] User :> "users" :> Post '[JSON] User
               :<|> "users" :> Capture "userId" Int :> Delete '[JSON] String

type Routes = Home
              :<|> "api" :> UserApi 
              :<|> "assets" :> Raw

