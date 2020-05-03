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
import Data.Types

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
  let flight = flightResolver res
  return flight
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
  let flights = map flightResolver res
  return flights
    where client' = client flightsAPI

type FlightAPI = "launches" :> Capture "id" Int :> Get '[JSON] Flight

flightAPI :: Proxy FlightAPI
flightAPI = Proxy

getFlight :: Int -> IO Flight
getFlight id = do
  manager' <- newManager tlsManagerSettings
  Right res <- runClientM client' (mkClientEnv manager' baseUrl')
  let flight = flightResolver res
  return flight
    where client' = client flightAPI id
