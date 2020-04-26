{-# LANGUAGE OverloadedStrings #-}

module Server.Scotty where

import Web.Scotty 
import Control.Monad.IO.Class
import API.Graphql

api :: IO ()
api = scotty 3000 $ do
  get "/" $ text "Hello world!" 
  get "/graphql" $ file "index.html"
  post "/graphql" $ raw =<< (liftIO . gqlAPI =<< body)
