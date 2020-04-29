{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module API.Graphql where

import Data.ByteString.Lazy.Char8
import Data.Text
import Data.Morpheus
import Data.Morpheus.Document
import Data.Morpheus.Types
import GHC.Generics

data Query m = Query
  { hello :: m Text
  } deriving (Generic, GQLType)

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = 
  GQLRootResolver
    { queryResolver = Query {hello}
    , mutationResolver = undefined
    , subscriptionResolver = undefined }
  where
    hello = pure "Hello world"

gqlAPI :: ByteString -> IO ByteString
gqlAPI = interpreter rootResolver
