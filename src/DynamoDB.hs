{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module DynamoDB where

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
    newGetItem,
    newKeySchemaElement,
    newPutItem,
    newQuery,
  )
import Amazonka.DynamoDB.Lens
  ( createTableResponse_tableDescription,
    createTable_attributeDefinitions,
    createTable_billingMode,
    deleteTableResponse_tableDescription,
    describeTableResponse_table,
    getItemResponse_item,
    getItem_key,
    putItem_item,
    queryResponse_items,
    query_expressionAttributeValues,
    query_keyConditionExpression,
  )
import Amazonka.DynamoDB.ListTables
import Amazonka.S3.ListBuckets
import Control.Lens
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty
import Data.Text (Text)

-- TODO:
--  - Change return type on functions to be JSON or some other structured format.
--      Make our own internal type?

exampleS3 :: AWS.Env -> IO ()
exampleS3 env = do
  --                                       new{Operation}
  resp <- AWS.runResourceT $ AWS.send env newListBuckets
  --              {operation}Response_{field}
  print $ resp ^. listBucketsResponse_buckets

exampleDynamoDBListTables :: AWS.Env -> IO ()
exampleDynamoDBListTables env = do
  resp <- AWS.runResourceT $ AWS.send env newListTables
  print $ resp ^. listTablesResponse_tableNames

exampleDynamoDBCreateTable :: AWS.Env -> IO ()
exampleDynamoDBCreateTable env = do
  resp <-
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
  print $ resp ^. createTableResponse_tableDescription

exampleDynamoDBDeleteTable :: AWS.Env -> IO ()
exampleDynamoDBDeleteTable env = do
  resp <-
    AWS.runResourceT $
      AWS.send env $
        newDeleteTable "MyTable"
  print $ resp ^. deleteTableResponse_tableDescription

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
  resp <-
    AWS.runResourceT $
      AWS.send env $
        newPutItem tableName & putItem_item .~ item
  print resp

exampleDynamoDBDescribeTable :: AWS.Env -> IO ()
exampleDynamoDBDescribeTable env = do
  respCreate <-
    AWS.runResourceT $
      AWS.send env $
        newDescribeTable "MyTable"
  print $ respCreate ^. describeTableResponse_table

exampleDynamoDBGetItem :: AWS.Env -> IO ()
exampleDynamoDBGetItem env = do
  respCreate <-
    AWS.runResourceT $
      AWS.send env $
        newGetItem
          "MyTable"
          & getItem_key .~ HM.fromList [("PrimaryKey", S "Foo")]
  print $ respCreate ^. getItemResponse_item

exampleDynamoDBQuery :: AWS.Env -> IO ()
exampleDynamoDBQuery env = do
  -- TODO: Add filter expressions
  respCreate <-
    AWS.runResourceT $
      AWS.send env $
        newQuery
          "MyTable"
          & query_keyConditionExpression ?~ "PrimaryKey = :pk"
          & query_expressionAttributeValues .~ pure (HM.fromList [(":pk", S "Foo")])
  print $ respCreate ^. queryResponse_items
