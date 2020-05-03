{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Data.Flight where

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
import Database.Spacex

resolveGetLatestFlight :: IORes e Flight
resolveGetLatestFlight = do
  flight' <- liftIO getLatestFlight
  let flight = flightResolver flight'
  return flight

resolveGetFlights :: IORes e [Flight]
resolveGetFlights = do
  flights' <- liftIO getFlights
  let flights = map flightResolver flights'
  return flights

data FlightArgs = FlightArgs
  { flightNumber :: Int
  } deriving (Generic)

resolveGetFlight :: FlightArgs -> IORes e Flight
resolveGetFlight FlightArgs { flightNumber } = do
  flight' <- liftIO $ getFlight flightNumber
  let flight = flightResolver flight'
  return flight
