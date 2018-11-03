{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- | 

module Users where

import Servant
import Data.Aeson.Compat
import Data.Aeson.Types
import GHC.Generics
import Elm (ElmType)

data User = User { username :: String
                 , age :: Int
                 , email :: String }
  deriving (Eq, Show, Generic, ElmType)

instance ToJSON User
instance FromJSON User
