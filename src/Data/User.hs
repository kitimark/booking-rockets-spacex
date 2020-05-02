{-# LANGUAGE OverloadedStrings #-}

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
import Data.Either

logindb :: String -> String -> IO ()
logindb user pass = do 
    conn <- open "data.db"
    userData <- liftIO $ queryUserByUsername user :: IO UserField
    close conn
    if validatePassword (B.pack $ password userData) (B.pack pass)
        then print $ decodeAndVerifySignature (hmacSecret "secret-key") (userToken userData)
        else print "fail"

userToken :: UserField -> T.Text
userToken userData = let 
    key = hmacSecret "secret-key"
    cs = mempty { 
        jti = stringOrURI $ T.pack $ username userData
        , unregisteredClaims =  ClaimsMap {unClaimsMap = Map.fromList [("id",Aeson.Number $ fromInteger $ userid userData)
                                                                    , ("username",Aeson.String $ T.pack $ username userData)
                                                                    , ("password",Aeson.String $ T.pack $ password userData)]}    
    }
    in encodeSigned key (JOSEHeader {typ = Just "JWT", cty = Nothing, alg = Just HS256, kid = Nothing}) cs

-- -- decodeToken :: T.Text -> 
-- decodeToken tokenText = let mJwt = decode tokenText in fmap header mJwt