{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Amazonka as AWS
import Amazonka.DynamoDB
  ( BillingMode (BillingMode_PAY_PER_REQUEST),
    KeyType (KeyType_HASH),
    ScalarAttributeType (ScalarAttributeType_S),
    newAttributeDefinition,
    newCreateTable,
    newKeySchemaElement,
  )
import Amazonka.DynamoDB.Lens
  ( createTableResponse_tableDescription,
    createTable_attributeDefinitions,
    createTable_billingMode,
  )
import Amazonka.DynamoDB.ListTables
import Amazonka.S3.ListBuckets
-- Amazonka's core functions

import Control.Lens -- For using lenses
import Data.List.NonEmpty
import qualified System.IO as IO

main :: IO ()
main =
  do
    -- A new Logger to replace the default noop logger is created, with the logger
    -- set to print debug information and errors to stdout:
    logger <- AWS.newLogger AWS.Debug IO.stdout

    -- To specify configuration preferences, newEnv is used to create a new
    -- configuration environment. The argument to newEnv is used to specify the
    -- mechanism for supplying or retrieving AuthN/AuthZ information.
    -- In this case discover will cause the library to try a number of options such
    -- as default environment variables, or an instance's IAM Profile and identity document:
    discoveredEnv <- AWS.newEnv AWS.discover

    let env =
          discoveredEnv
            { AWS.logger = logger
            }
    -- env <- AWS.newEnv AWS.discover
    exampleS3 env
    exampleDynamoDBListTables env -- List tables. We expect Just [].
    exampleDynamoDBCreateTable env -- Create a table. Called "foo"
    exampleDynamoDBListTables env -- List tables. We expect Just ["foo"].

-- Library functions

exampleS3 :: AWS.Env -> IO ()
exampleS3 env = do
  --                                       new{Operation}
  resp <- AWS.runResourceT $ AWS.send env newListBuckets
  --              {operation}Response_{field}
  print $ resp ^. listBucketsResponse_buckets

exampleDynamoDBListTables :: AWS.Env -> IO ()
exampleDynamoDBListTables env = do
  respList <- AWS.runResourceT $ AWS.send env newListTables
  print $ respList ^. listTablesResponse_tableNames

exampleDynamoDBCreateTable :: AWS.Env -> IO ()
exampleDynamoDBCreateTable env = do
  respCreate <-
    AWS.runResourceT $
      AWS.send env $
        newCreateTable "MyTable" (newKeySchemaElement "Id" KeyType_HASH :| [])
          & createTable_attributeDefinitions .~ [newAttributeDefinition "Id" ScalarAttributeType_S]
          & createTable_billingMode ?~ BillingMode_PAY_PER_REQUEST
  print $ respCreate ^. createTableResponse_tableDescription
