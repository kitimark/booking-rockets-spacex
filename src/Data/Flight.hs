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
resolveGetLatestFlight = liftIO getLatestFlight

resolveGetFlights :: IORes e [Flight]
resolveGetFlights = liftIO getFlights

data FlightArgs = FlightArgs
  { flightNumber :: Int
  } deriving (Generic)

resolveGetFlight :: FlightArgs -> IORes e Flight
resolveGetFlight FlightArgs { flightNumber } = liftIO $ getFlight flightNumber
