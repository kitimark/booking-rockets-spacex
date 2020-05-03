module Main where

import Server.Scotty
import Database.Sqlite

main :: IO ()
main = do 
  initdb
  api
