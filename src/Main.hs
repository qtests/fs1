{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- API
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}


module Main where

import Yesod

-- API
import Data.Text (Text)
import Network.Wai
import Servant
import Yesod.Core.Types


type AppAPI = "items" :> Get '[JSON] Value


appAPIServerMock :: Server AppAPI
appAPIServerMock = return $ toJSON [ object [ "id" .= (1 :: Int)
                                            , "name" .= ("one" :: Text)
                                            ]
                                   , object [ "id" .= (2 :: Int)
                                            , "name" .= ("two" :: Text)
                                            ]
                                   , object [ "id" .= (3 :: Int)
                                            , "name" .= ("three" :: Text)
                                            ]
                                   ]
                              
appAPIProxy :: Proxy AppAPI
appAPIProxy = Proxy

data EmbeddedAPI = EmbeddedAPI { eapiApplication :: Application
                               }
             
instance RenderRoute EmbeddedAPI where
  data Route EmbeddedAPI = EmbeddedAPIR ([Text], [(Text, Text)])
    deriving(Eq, Show, Read)
  renderRoute (EmbeddedAPIR t) = t

instance ParseRoute EmbeddedAPI where
  parseRoute t = Just (EmbeddedAPIR t)

  
instance Yesod master => YesodSubDispatch EmbeddedAPI master where
  yesodSubDispatch YesodSubRunnerEnv{..} req = resp
    where
      master = yreSite ysreParentEnv
      site = ysreGetSub master
      resp = eapiApplication site req

-- Yesod part

data App = App { appAPI :: EmbeddedAPI }
instance Yesod App

mkYesod "App" [parseRoutes|
  / HomeR GET
  /api/v1/ SubsiteR EmbeddedAPI appAPI
  |]

getHomeR :: Main.Handler Html
getHomeR = defaultLayout $ do

  -- let filenames = ["readme.txt", "report.pdf", "music.wav"] :: [String]
  let filenames = [] :: [String]
  let itemsApiRoute = SubsiteR (EmbeddedAPIR (["items"], []))

  setTitle "File Processor"

  toWidget [whamlet|
    <h2>Previously submitted files

    $# 
    $# <ul>
    $#    $forall filename <- filenames
    $#        <li>#{filename}

    $if null filenames
      <p>No files have been uploaded yet.
    $else
      <ul>
      $forall filename <- filenames
          <li>#{filename}

    <p> 
      Try testing our items API at
      <a href=@{itemsApiRoute}>@{itemsApiRoute}
  |]

-- http://localhost:3000/
main :: IO ()
-- main = warp 3000 App
main = do
  let api = serve appAPIProxy appAPIServerMock
  warp 3000 (App (EmbeddedAPI api))

