{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | 

module Db where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Users (User( User ), username, age, email)


withConn :: String -> (Connection -> IO a) -> IO a
withConn dbName action = do
  conn <- open dbName
  res <- action conn
  close conn
  return res


instance FromRow User where
  fromRow = User
            <$> field
            <*> field
            <*> field

createUser :: User -> IO ()
createUser User{..} = do
  conn <- open "db.db" 
  execute conn "INSERT INTO USERS (username, age, email) values (?, ?, ?)" (username, age, email)
  close conn

getUsers :: IO [User]
getUsers = withConn "db.db" $
  \conn -> do
    query_ conn "SELECT username, age, email FROM users;" :: IO [User]
