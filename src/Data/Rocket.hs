{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Rocket where 

import Data.Aeson
import Data.Proxy
import Data.Text
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

data RocketResponse = RocketResponse
  { rocketId :: Text
  , rocketName :: Text
  , rocketType :: Text
  } deriving (Eq, Show)

data FlightResponse = FlightResponse
  { flightNumber :: Int
  , missionName :: Text 
  , rocket :: RocketResponse
  } deriving (Eq, Show)

instance FromJSON FlightResponse where
  parseJSON = withObject "" $ \o -> do
    fid <- o .: "flight_number"
    name <- o .: "mission_name"
    r <- o .: "rocket"
    rid <- r .: "rocket_id"
    rname <- r .: "rocket_name"
    rtype <- r .: "rocket_type"
    return $ FlightResponse fid name $
      (RocketResponse rid rname rtype)

type SpacexAPI = "v3" 
  :> "launches" 
  :> "latest"
  :> Get '[JSON] FlightResponse

spacexAPI :: Proxy SpacexAPI
spacexAPI = Proxy

queryLatestFlight :: ClientM FlightResponse
queryLatestFlight = client spacexAPI

getLatestFlight :: IO FlightResponse
getLatestFlight = do
  let baseUrl' = BaseUrl Https "api.spacexdata.com" 443 ""
  manager' <- newManager tlsManagerSettings
  Right res <- runClientM queryLatestFlight $
    (mkClientEnv manager' baseUrl')
  return res
