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
import Data.Morpheus
import Data.Morpheus.Document
import Data.Morpheus.Types
import GHC.Generics

logindb :: String -> String -> IO T.Text
logindb user pass = do 
  conn <- open "data.db"
  userData <- liftIO $ queryUserByUsername user :: IO UserField
  let passwordHash' = (password :: UserField -> String) userData
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
    , ("username",Aeson.String $ T.pack $ (username :: UserField -> String) userData)
    , ("password",Aeson.String $ T.pack $ (password :: UserField -> String) userData) ]
  cs = mempty 
    { jti = stringOrURI $ T.pack $ (username :: UserField -> String) userData
    , unregisteredClaims =  ClaimsMap {unClaimsMap = payload}    
    }
  in encodeSigned key header cs

-- -- decodeToken :: T.Text -> 
-- decodeToken tokenText = let mJwt = decode tokenText in fmap header mJwt

data CredentialArgs = CredentialArgs
  { username :: T.Text
  , password :: T.Text 
  } deriving (Generic)

resolveUserLogin :: CredentialArgs -> IOMutRes e T.Text
resolveUserLogin credential = do
  let user = T.unpack $ (username :: CredentialArgs -> T.Text) credential
  let pass = T.unpack $ (password :: CredentialArgs -> T.Text) credential
  liftIO $ logindb user pass