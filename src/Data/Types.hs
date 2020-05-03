{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Data.Types where 

import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Data.Morpheus.Types
import Database.Sqlite
import Database.Spacex
import Foreign.Marshal.Unsafe

data Rocket = Rocket
  { rocketId :: Text
  , rocketName :: Text
  , rocketType :: Text
  } deriving (Generic, GQLType)

data Flight = Flight
  { flightNumber :: Int
  , missionName :: Text 
  , rocket :: Rocket
  , bookedUsers :: [User]
  } deriving (Generic, GQLType)

reduceFlightByUserId :: Int -> [Flight]
reduceFlightByUserId userId = do
  let booksField = unsafeLocalState $ queryBookingByUserID userId
  let flightIds = map mapFlightId booksField
  let flights' = map getFlightUnsafe flightIds
  map flightResolver flights'
  where 
    mapFlightId :: BookField -> Int
    mapFlightId BookField { flightNumber } = flightNumber
    getFlightUnsafe :: Int -> FlightResponse
    getFlightUnsafe flightNumber = unsafeLocalState $ getFlight flightNumber


reduceUserByBook :: BookField -> User
reduceUserByBook BookField { userID } = User
  { userId = userID 
  , username = (username :: UserField -> Text) user'
  , password = (password :: UserField -> Text) user'
  , bookings = reduceFlightByUserId userID
  } where 
    user' = unsafeLocalState $ queryUserByID userID

flightResolver :: FlightResponse -> Flight
flightResolver FlightResponse {flightNumber, missionName, rocket} = Flight 
  { flightNumber = flightNumber
  , missionName = missionName
  , rocket = rocket'
  , bookedUsers = bookedUsers' 
  } where
    rocket' = Rocket 
      { rocketId = (rocketId :: RocketResponse -> Text) rocket
      , rocketName = (rocketName :: RocketResponse -> Text) rocket
      , rocketType = (rocketType :: RocketResponse -> Text) rocket
      }
    bookedUsers' = do
      let users' = unsafeLocalState $ queryBookingByFlightID flightNumber
      map reduceUserByBook users'

data User = User
  { userId :: Int
  , username :: Text
  , password :: Text
  , bookings :: [Flight]
  } deriving (Generic, GQLType)

userResolver :: UserField -> User
userResolver UserField { userid, username, password } = User 
  { userId = userid
  , username
  , password
  , bookings = reduceFlightByUserId userid
  } 
