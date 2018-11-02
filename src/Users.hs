{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | 

module Users where

import Servant
import Data.Aeson.Compat
import Data.Aeson.Types
import GHC.Generics
import Elm (ElmType)

type UserAPI = "users" :> Get '[JSON] [User]

data User = User { name :: String
                 , age :: Int
                 , email :: String }
  deriving (Eq, Show, Generic, ElmType)

instance ToJSON User

users :: [User]
users =
  [ User "Isaac" 375 "isaac@newton.com" 
  , User "Alber" 134 "ae@mc2.com" 
  ]

