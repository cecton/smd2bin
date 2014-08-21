#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


main :: IO ()
main = serve Nothing myApp


myApp :: ServerPart Response
myApp = msum
  [ dir "files"   $ fileServing
  , upload
  ]


template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
      H.link ! A.rel "stylesheet"
             ! A.media "screen"
             ! A.href "files/bootstrap/css/bootstrap.min.css"
    H.body ! A.style "padding-top: 60px; padding-bottom: 40px;" $ do
      H.script ! A.src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
               $ ""
      H.div ! A.class_ "container" $ body


fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing ["index.html"] "."


upload :: ServerPart Response
upload =
       msum [ uploadForm
            , handleUpload
            ]
    where
    uploadForm :: ServerPart Response
    uploadForm =
        do method GET
           ok $ template "upload form" $ do
            H.div ! A.class_ "hero-unit" $ do
              H.h1 "smd2bin"
              H.p $ "Convert your Genesis SMD ROM file to original BIN format."
              H.form ! A.enctype "multipart/form-data" ! A.method "POST"
                     ! A.action "/upload" ! A.class_ "text-center" $ do
                H.input ! A.type_ "file"
                        ! A.name "file_upload"
                        ! A.class_ "hide hidden"
                        ! A.id "file"
                H.button ! A.type_ "button"
                         ! A.class_ "btn btn-large btn-block btn-primary"
                         ! A.id "select"
                         $ "select a file"
                H.button ! A.type_ "submit"
                         ! A.class_ "btn btn-large btn-block btn-primary"
                         $ "upload"
            H.script $
                "$(\"#select\").click(function(){$(\"#file\" ).click()});"

    handleUpload :: ServerPart Response
    handleUpload =
        do (tmpFile, uploadName, contentType) <- lookFile "file_upload"
           ok $ template "file uploaded" $ do
                H.p (toHtml $ "temporary file: " ++ tmpFile)
                H.p (toHtml $ "uploaded name:  " ++ uploadName)
                H.p (toHtml $ "content-type:   " ++ show contentType)
