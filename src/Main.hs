{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod

data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
  / HomeR GET
  |]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do

  -- let filenames = ["readme.txt", "report.pdf", "music.wav"] :: [String]
  let filenames = [] :: [String]
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
  |]

-- http://localhost:3000/
main :: IO ()
main = warp 3000 App

