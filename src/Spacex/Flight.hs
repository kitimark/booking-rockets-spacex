{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Spacex.Flight where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text
import Data.Morpheus
import Data.Morpheus.Document
import Data.Morpheus.Types
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import GHC.Generics

data Rocket = Rocket
  { rocketId :: Text
  , rocketName :: Text
  , rocketType :: Text
  } deriving (Generic, GQLType)

instance FromJSON Rocket where
  parseJSON = withObject "rocket" $ \o -> do
    r_id <- o .: "rocket_id"
    r_name <- o .: "rocket_name"
    r_type <- o .: "rocket_type"
    return $ Rocket r_id r_name r_type

data Flight = Flight
  { flightNumber :: Int
  , missionName :: Text 
  , rocket :: Rocket
  } deriving (Generic, GQLType)

instance FromJSON Flight where
  parseJSON = withObject "" $ \o -> do
    f_num <- o .: "flight_number" 
    m_name <- o .: "mission_name"
    rocket <- o .: "rocket"
    return $ Flight f_num m_name rocket

baseUrl' :: BaseUrl
baseUrl' = BaseUrl Https "api.spacexdata.com" 443 "v3"

type LastestFlightAPI = "launches" :> "latest"
  :> Get '[JSON] Flight

latestFlightAPI :: Proxy LastestFlightAPI
latestFlightAPI = Proxy

getLatestFlight :: IO Flight
getLatestFlight = do
  manager' <- newManager tlsManagerSettings
  Right res <- runClientM client' (mkClientEnv manager' baseUrl') 
  return res
    where client' = client latestFlightAPI

resolveGetLatestFlight :: IORes e Flight
resolveGetLatestFlight = liftIO getLatestFlight

type FlightsAPI = "launches" :> Get '[JSON] [Flight]

flightsAPI :: Proxy FlightsAPI
flightsAPI = Proxy

getFlights :: IO [Flight]
getFlights = do
  manager' <- newManager tlsManagerSettings
  Right res <- runClientM client' (mkClientEnv manager' baseUrl')
  return res
    where client' = client flightsAPI

resolveGetFlights :: IORes e [Flight]
resolveGetFlights = liftIO getFlights

type FlightAPI = "launches" :> Capture "id" Int :> Get '[JSON] Flight

flightAPI :: Proxy FlightAPI
flightAPI = Proxy

getFlight :: Int -> IO Flight
getFlight id = do
  manager' <- newManager tlsManagerSettings
  Right res <- runClientM client' (mkClientEnv manager' baseUrl')
  return res
    where client' = client flightAPI id

data FlightArgs = FlightArgs
  { flightNumber :: Int
  } deriving (Generic)

resolveGetFlight :: FlightArgs -> IORes e Flight
resolveGetFlight FlightArgs { flightNumber } = liftIO $ getFlight flightNumber
