{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Data.User where

import qualified Data.ByteString.Char8 as B
import Crypto.BCrypt
import Database.Sqlite
import Database.SQLite.Simple
import qualified Data.Map as Map
import Data.Maybe
import Web.JWT
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

logindb :: String -> String -> IO T.Text
logindb user pass = do 
  conn <- open "data.db"
  userData <- liftIO $ queryUserByUsername user :: IO UserField
  let passwordHash' = password userData
  close conn
  if validatePassword (B.pack passwordHash') (B.pack pass)
    -- then print $ decodeAndVerifySignature (hmacSecret "secret-key") (userToken userData)
    then return $ userToken userData
    else error "Unauthorized"

userToken :: UserField -> T.Text
userToken userData = let 
  key = hmacSecret "secret-key"
  header = JOSEHeader 
    { typ = Just "JWT"
    , cty = Nothing
    , alg = Just HS256
    , kid = Nothing }
  payload = Map.fromList 
    [ ("id",Aeson.Number $ fromInteger $ userid userData)
    , ("username",Aeson.String $ T.pack $ username userData)
    , ("password",Aeson.String $ T.pack $ password userData) ]
  cs = mempty 
    { jti = stringOrURI $ T.pack $ username userData
    , unregisteredClaims =  ClaimsMap {unClaimsMap = payload}    
    }
  in encodeSigned key header cs

-- -- decodeToken :: T.Text -> 
-- decodeToken tokenText = let mJwt = decode tokenText in fmap header mJwt