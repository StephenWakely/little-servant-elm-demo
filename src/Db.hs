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
            <*> field

  

createUser :: User -> IO Int
createUser User{..} = withConn "db.db" $
  \conn -> do
    execute conn "INSERT INTO USERS (username, age, email) values (?, ?, ?)" (username, age, email)
    id <- lastInsertRowId conn
    return $ fromIntegral id


deleteUser :: Int -> IO ()
deleteUser id = do
  conn <- open "db.db"
  execute conn "DELETE FROM USERS where id = (?)" (Only id)
  close conn

getUsers :: IO [User]
getUsers = withConn "db.db" $
  \conn -> do
    query_ conn "SELECT id, username, age, email FROM users;" :: IO [User]
