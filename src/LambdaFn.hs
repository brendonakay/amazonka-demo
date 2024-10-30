{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module LambdaFn where

import Aws.Lambda
import Data.Aeson
import GHC.Generics

-- TODO:
--   - Learn more about ALB and API Gateway

data Person = Person
  { name :: String,
    age :: Int
  } -- We kindly ask the compiler to autogenerate JSON instances for us
  deriving (Generic, FromJSON, ToJSON)

handler :: Person -> Context () -> IO (Either String Person)
handler person _context =
  pure (handlerFn person)

-- gatewayHandler ::
--  ApiGatewayRequest request ->
--  Context context ->
--  IO (Either (ApiGatewayResponse error) (ApiGatewayResponse response))
-- gatewayHandler = doSomething
--
-- albHandler ::
--  ALBRequest request ->
--  Context context ->
--  IO (Either (ALBResponse error) (ALBResponse response))
-- albHandler = doSomething
--
-- regularHandler ::
--  request ->
--  Context context ->
--  IO (Either error response)
-- regularHandler = doSomething

handlerFn :: Person -> Either String Person
handlerFn person =
  if age person > 0
    then Right person
    else Left "A person's age must be positive"
