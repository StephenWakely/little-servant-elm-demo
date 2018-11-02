{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 

module Home where

import Servant
import Lucid                    (Html, body_, content_, doctypehtml_,
                                 head_, href_, link_, meta_, name_,
                                 rel_, script_, src_, title_)
import Servant.HTML.Lucid       (HTML)

type Home = Get '[HTML] (Html ())

homePage :: Html ()
homePage =
  doctypehtml_ $ do
    head_ $ do
      title_ "Groovy servant"
      meta_ [ name_ "viewport"
            , content_ "width=device-width, initial-scale=1" ]
      link_ [ rel_ "stylesheet"
             , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
             ]
      script_ [src_ "assets/app.js" ] ("" :: String)
    body_ (script_ "var elmApp = Elm.Main.fullscreen()")


