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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Morpheus
import Data.Morpheus.Document
import Data.Morpheus.Types
import GHC.Generics
import Data.Flight
import Foreign.Marshal.Unsafe

logindb :: String -> String -> IO T.Text
logindb user pass = do 
  conn <- open "data.db"
  userData <- liftIO $ queryUserByUsername user :: IO UserField
  let passwordHash' = (password :: UserField -> Text) userData
  close conn
  if validatePassword (B.pack $ T.unpack passwordHash') (B.pack pass)
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
    -- [ ("id", Aeson.Number $ fromIntegral userid userData)
    [ ("username",Aeson.String $ (username :: UserField -> Text) userData)
    , ("password",Aeson.String $ (password :: UserField -> Text) userData) ]
  cs = mempty 
    { jti = stringOrURI $ (username :: UserField -> Text) userData
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

data User = User
  { userId :: Int
  , username :: Text
  , password :: Text
  , bookings :: [Flight]
  } deriving (Generic, GQLType)

resolveGetUsers :: IORes e [User]
resolveGetUsers = do
  users <- liftIO $ queryUsers
  let users' = map reduceUser users
  return users'

data GetUserArgs = GetUserArgs
  { userId :: Int
  } deriving (Generic)

resolveGetUser :: GetUserArgs -> IORes e User
resolveGetUser GetUserArgs { userId } = do
  user <- liftIO $ queryUserByID userId
  let user' = reduceUser user
  return user'


reduceUser :: UserField -> User
reduceUser UserField{userid, username, password} = User 
  { userId = userid
  , username = username
  , password = password 
  , bookings = bookings'
  } where 
    bookings' = do
      let bookingField = unsafeLocalState $ queryBookingByUserID userid
      map reduceFlight bookingField

reduceFlight :: BookField -> Flight
reduceFlight BookField { flightNumber } = 
  unsafeLocalState $ getFlight flightNumber 
