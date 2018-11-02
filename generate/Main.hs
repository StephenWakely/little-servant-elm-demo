{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Proxy  (Proxy (Proxy))
import           Elm         (Spec (Spec), specsToDir, toElmTypeSource,
                              toElmDecoderSource, toElmEncoderSource)
import           Servant.Elm (ElmOptions (..), defElmImports, defElmOptions,
                              generateElmForAPIWith, UrlPrefix (Static))

import           Lib  (UserAPI, User)

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "http://localhost:8000/api" }

specs :: [Spec]
specs =
  [ Spec ["Generated", "Api"]
         (defElmImports
          : toElmTypeSource    (Proxy :: Proxy User)
          : toElmDecoderSource (Proxy :: Proxy User)
          : toElmEncoderSource (Proxy :: Proxy User)
          : generateElmForAPIWith elmOpts  (Proxy :: Proxy UserAPI))
  ]

main :: IO ()
main = specsToDir specs "frontend/src"
