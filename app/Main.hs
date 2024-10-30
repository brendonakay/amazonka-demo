{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Amazonka
import Aws.Lambda
import DynamoDB
import qualified LambdaFn
import qualified System.IO as IO

-- TODO:
--  - Pretty print output from IO functions. They usually return JSON.

main :: IO ()
main =
  do
    dynamoDBMain

dynamoDBMain :: IO ()
dynamoDBMain = do
  -- From the docs:
  -- A new Logger to replace the default noop logger is created, with the logger
  -- set to print debug information and errors to stdout:
  logger' <- Amazonka.newLogger Amazonka.Debug IO.stdout

  -- To specify configuration preferences, newEnv is used to create a new
  -- configuration environment. The argument to newEnv is used to specify the
  -- mechanism for supplying or retrieving AuthN/AuthZ information.
  -- In this case discover will cause the library to try a number of options such
  -- as default environment variables, or an instance's IAM Profile and identity document:
  discoveredEnv <- Amazonka.newEnv Amazonka.discover

  let env =
        discoveredEnv
          { Amazonka.logger = logger'
          }

  -- Run the lib functions
  putStrLn "\n\nRunning exampleDynamoDBListTables"
  exampleDynamoDBListTables env

-- putStrLn "\n\nRunning exampleDynamoDBDeleteTable"
-- exampleDynamoDBDeleteTable env
-- putStrLn "\n\nRunning exampleDynamoDBCreateTable"
-- exampleDynamoDBCreateTable env
-- putStrLn "\n\nRunning exampleDynamoDBGetItem"
-- exampleDynamoDBGetItem env
-- putStrLn "\n\nRunning exampleDynamoDBQuery"
-- exampleDynamoDBQuery env

awsLambdaMain :: IO ()
awsLambdaMain =
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id
    $ do
      -- You could also register multiple handlers
      -- addStandaloneLambdaHandler "handler" LambdaFn.handler
      -- addAPIGatewayHandler "api-gateway" LambdaFn.gatewayHandler
      -- addALBHandler "alb" LambdaFn.albHandler
      addStandaloneLambdaHandler "standalone" LambdaFn.handler
