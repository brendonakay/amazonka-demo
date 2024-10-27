{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Amazonka as AWS
import DynamoDB
import qualified System.IO as IO

-- TODO:
--  - Pretty print output from IO functions. They usually return JSON.

main :: IO ()
main =
  do
    -- A new Logger to replace the default noop logger is created, with the logger
    -- set to print debug information and errors to stdout:
    logger' <- AWS.newLogger AWS.Debug IO.stdout

    -- To specify configuration preferences, newEnv is used to create a new
    -- configuration environment. The argument to newEnv is used to specify the
    -- mechanism for supplying or retrieving AuthN/AuthZ information.
    -- In this case discover will cause the library to try a number of options such
    -- as default environment variables, or an instance's IAM Profile and identity document:
    discoveredEnv <- AWS.newEnv AWS.discover

    let env =
          discoveredEnv
            { AWS.logger = logger'
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
