{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Amazonka as AWS
import Amazonka.DynamoDB
  ( AttributeValue (S),
    BillingMode (BillingMode_PAY_PER_REQUEST),
    KeyType (KeyType_HASH, KeyType_RANGE),
    ScalarAttributeType (ScalarAttributeType_S),
    newAttributeDefinition,
    newCreateTable,
    newDeleteTable,
    newDescribeTable,
    newKeySchemaElement,
    newPutItem,
  )
import Amazonka.DynamoDB.Lens
  ( createTableResponse_tableDescription,
    createTable_attributeDefinitions,
    createTable_billingMode,
    deleteTableResponse_tableDescription,
    describeTableResponse_table,
    putItemResponse_attributes,
    putItem_item,
  )
import Amazonka.DynamoDB.ListTables
import Amazonka.S3.ListBuckets
import Control.Lens
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty
import Data.Text (Text)
import qualified System.IO as IO

-- Amazonka's core functions
-- TODO:
--  - Pretty print output from IO functions. They usually return JSON.
--  - Move everything to modules.

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
    -- putStrLn "\n\nRunning example3"
    -- exampleS3 env
    putStrLn "\n\nRunning exampleDynamoDBListTables"
    exampleDynamoDBListTables env -- List tables. We expect Just [].
    -- putStrLn "\n\nRunning exampleDynamoDBDeleteTable"
    -- exampleDynamoDBDeleteTable env -- Delete the table.
    -- putStrLn "\n\nRunning exampleDynamoDBCreateTable"
    -- exampleDynamoDBCreateTable env -- Create a table. Called "foo"
    -- putStrLn "\n\nRunning exampleDynamoDBListTables"
    -- exampleDynamoDBListTables env -- List tables. We expect Just ["foo"].
    -- putStrLn "\n\nRunning exampleDynamoDBDeleteTable"
    -- exampleDynamoDBDeleteTable env -- Delete the table.
    -- putStrLn "\n\nRunning exampleDynamoDBListTables"
    -- exampleDynamoDBListTables env -- List tables. We expect Just [].

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
        newCreateTable
          -- Table name
          "MyTable"
          -- Create Key Schema
          (newKeySchemaElement "PrimaryKey" KeyType_HASH :| [newKeySchemaElement "SortKey" KeyType_RANGE])
          -- Create Attribute Definitions
          & createTable_attributeDefinitions
            .~ [ newAttributeDefinition "PrimaryKey" ScalarAttributeType_S,
                 newAttributeDefinition "SortKey" ScalarAttributeType_S
               ]
          -- Set Billing Mode
          & createTable_billingMode ?~ BillingMode_PAY_PER_REQUEST
  print $ respCreate ^. createTableResponse_tableDescription

exampleDynamoDBDeleteTable :: AWS.Env -> IO ()
exampleDynamoDBDeleteTable env = do
  respCreate <-
    AWS.runResourceT $
      AWS.send env $
        newDeleteTable "MyTable"
  print $ respCreate ^. deleteTableResponse_tableDescription

exampleDynamoDBPutItem ::
  AWS.Env ->
  -- TODO: Alias these parameter types
  Text -> -- tableName
  Text -> -- keyName
  Text -> -- keyValue
  Text -> -- attributeName
  Text -> -- attributeValue
  IO ()
exampleDynamoDBPutItem env tableName keyName keyValue attributeName attributeValue = do
  let item =
        -- Hash Map of the data to be put
        HM.fromList
          [ (keyName, S keyValue),
            (attributeName, S attributeValue)
          ]
  respCreate <-
    AWS.runResourceT $
      AWS.send env $
        newPutItem tableName & putItem_item .~ item
  print $ respCreate ^. putItemResponse_attributes

exampleDynamoDBDescribeTable :: AWS.Env -> IO ()
exampleDynamoDBDescribeTable env = do
  respCreate <-
    AWS.runResourceT $
      AWS.send env $
        newDescribeTable "MyTable"
  print $ respCreate ^. describeTableResponse_table

-- TODO: Querying data
