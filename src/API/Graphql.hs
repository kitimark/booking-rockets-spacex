{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module API.Graphql where

import Data.ByteString.Lazy.Char8
import Data.Text
import Data.Morpheus
import Data.Morpheus.Document
import Data.Morpheus.Types

importGQLDocumentWithNamespace "schema.gql"

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = 
  GQLRootResolver
    { queryResolver = Query {queryHello}
    , mutationResolver = undefined
    , subscriptionResolver = undefined }
  where
    queryHello = pure "Hello world"

gqlAPI :: ByteString -> IO ByteString
gqlAPI = interpreter rootResolver
