{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module API.Graphql where

import Data.ByteString.Lazy.Char8
import Data.Text
import Data.Morpheus
import Data.Morpheus.Document
import Data.Morpheus.Types
import Control.Monad.IO.Class (liftIO)
import qualified Spacex.Rocket as R

importGQLDocument "schema.gql"

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = 
  GQLRootResolver
    { queryResolver = Query {hello, getLatestFlight}
    , mutationResolver = undefined
    , subscriptionResolver = undefined }
  where
    hello = pure "Hello world"
    getLatestFlight = liftIO $ do 
      flight' <- R.getLatestFlight
      let rocketRes = (R.rocket flight')
      let rocket' = pure Rocket { 
        rocketId = pure (R.rocketId rocketRes),
        rocketName = pure (R.rocketName rocketRes),
        rocketType = pure (R.rocketType rocketRes) }
      pure Flight 
        { flightNumber = pure (R.flightNumber flight')
        , missionName = pure (R.missionName flight')
        , rocket = rocket' }

gqlAPI :: ByteString -> IO ByteString
gqlAPI = interpreter rootResolver
