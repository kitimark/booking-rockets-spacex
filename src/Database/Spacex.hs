{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.Spacex where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import Data.Morpheus
import Data.Morpheus.Document
import Data.Morpheus.Types
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import GHC.Generics
import Foreign.Marshal.Unsafe
import Database.Sqlite

data FlightResponse = FlightResponse
  { flightNumber :: Int
  , missionName :: Text 
  , rocket :: RocketResponse
  } deriving (Generic, Eq)

instance FromJSON FlightResponse where
  parseJSON = withObject "" $ \o -> do
    f_num <- o .: "flight_number" 
    m_name <- o .: "mission_name"
    rocket <- o .: "rocket"
    return FlightResponse
      { flightNumber = f_num 
      , missionName = m_name 
      , rocket = rocket }

data RocketResponse = RocketResponse
  { rocketId :: Text
  , rocketName :: Text
  , rocketType :: Text
  } deriving (Generic, Eq)

instance FromJSON RocketResponse where
  parseJSON = withObject "rocket" $ \o -> do
    r_id <- o .: "rocket_id"
    r_name <- o .: "rocket_name"
    r_type <- o .: "rocket_type"
    return RocketResponse
      { rocketId = r_id
      , rocketName = r_name 
      , rocketType = r_type }

baseUrl' :: BaseUrl
baseUrl' = BaseUrl Https "api.spacexdata.com" 443 "v3"

type LastestFlightAPI = "launches" :> "latest"
  :> Get '[JSON] FlightResponse

latestFlightAPI :: Proxy LastestFlightAPI
latestFlightAPI = Proxy

getLatestFlight :: IO FlightResponse
getLatestFlight = do
  manager' <- newManager tlsManagerSettings
  Right res <- runClientM client' (mkClientEnv manager' baseUrl') 
  return res
    where client' = client latestFlightAPI

type FlightsAPI = "launches" :> Get '[JSON] [FlightResponse]

flightsAPI :: Proxy FlightsAPI
flightsAPI = Proxy

getFlights :: IO [FlightResponse]
getFlights = do
  manager' <- newManager tlsManagerSettings
  Right res <- runClientM client' (mkClientEnv manager' baseUrl')
  return res
    where client' = client flightsAPI

type FlightAPI = "launches" :> Capture "id" Int :> Get '[JSON] FlightResponse

flightAPI :: Proxy FlightAPI
flightAPI = Proxy

getFlight :: Int -> IO FlightResponse
getFlight id = do
  manager' <- newManager tlsManagerSettings
  Right res <- runClientM client' (mkClientEnv manager' baseUrl')
  return res
    where client' = client flightAPI id
