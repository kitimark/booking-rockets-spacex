{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module API.Graphql where

import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8
import Data.Text
import Data.Morpheus
import Data.Morpheus.Document
import Data.Morpheus.Types
import GHC.Generics
import Data.Flight

data Query m = Query
  { hello :: m Text
  , getLatestFlight :: m Flight
  , getFlights :: m [Flight]
  , getFlight :: FlightArgs -> m Flight
  } deriving (Generic, GQLType)

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = 
  GQLRootResolver
    { queryResolver = Query 
      { hello
      , getLatestFlight = resolveGetLatestFlight
      , getFlights = resolveGetFlights 
      , getFlight = resolveGetFlight }
    , mutationResolver = undefined
    , subscriptionResolver = undefined }
  where
    hello = pure "Hello world"

gqlAPI :: ByteString -> IO ByteString
gqlAPI = interpreter rootResolver
  