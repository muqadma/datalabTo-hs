{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TupleSections, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, LambdaCase  #-}
{-|
Module : Fast
-}


module Fast
  (  module Fast.API
  , module Fast.Client
  , module Fast.Core
  , module Fast.Logging
  , module Fast.MimeTypes
  , module Fast.Model
  , module Fast.ModelLens
  , withConfig
  , pollFinal
  , ocr'
  , layout'
  , lineDetection'
  , marker'
  , ocr
  ) where

import Fast.API
import Fast.Client
import Fast.Core
import Fast.Logging
import Fast.MimeTypes
import Fast.Model
import Fast.ModelLens

import Network.HTTP.Client as NH
import Network.HTTP.Client.TLS as NH
import Data.Text (Text)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T

config :: Text -> IO (FastConfig, Manager)
config token = (,)
  <$> (flip addAuthMethod (AuthApiKeyAPIKeyHeader token) <$> newConfig)
  <*> newManager tlsManagerSettings

handleInitialResponse :: InitialResponse -> IO (Maybe (RequestId, Text))
handleInitialResponse ir = do
  putStrLn "Initial Response ready!"
  let checkUrl = initialResponseRequestCheckUrl ir
  let requestID = RequestId $ initialResponseRequestId ir
  case (initialResponseSuccess ir) of
    Just True -> do
      putStrLn "Success!"
      putStrLn $ "Check URL: " ++ show checkUrl
      putStrLn $ "Request ID: " ++ show requestID
      return $ Just (requestID, checkUrl)
    _ -> do
      putStrLn "Failure!"
      putStrLn $ "Check URL: " ++ show checkUrl
      putStrLn $ "Request ID: " ++ show requestID
      case initialResponseError ir of
        Just e -> do
          putStrLn $ "Error: " ++ show e
          return Nothing
        Nothing -> do
          putStrLn "No error message"
          return Nothing

type FastEnv = (FastConfig, Manager)

withConfig :: Text -> (FastEnv -> IO a) -> IO a
withConfig token f = f =<< config token

ocr :: FastEnv -> [FilePath] -> IO [Maybe OCRFinalResponse]
ocr env fs = mapM (ocr' env) fs

ocr' :: FastEnv -> FilePath -> IO (Maybe OCRFinalResponse)
ocr' env f = pollFinal env (File f) p ocrResponseApiV1OcrRequestIdGet oCRFinalResponseStatus oCRFinalResponseError
  where
    p = flip applyOptionalParam (Langs "english,urdu") . ocrApiV1OcrPost

layout' :: FastEnv -> FilePath -> IO (Maybe LayoutFinalResponse)
layout' env f = pollFinal env (File f) layoutApiV1LayoutPost layoutResponseApiV1LayoutRequestIdGet layoutFinalResponseStatus layoutFinalResponseError

lineDetection' :: FastEnv -> FilePath -> IO (Maybe LineDetectionFinalResponse)
lineDetection' env f = pollFinal env (File f) lineDetectionApiV1LineDetectionPost lineDetectionResponseApiV1LineDetectionRequestIdGet lineDetectionFinalResponseStatus lineDetectionFinalResponseError

marker' :: FastEnv -> FilePath -> IO (Maybe MarkerFinalResponse)
marker' env f = pollFinal env (File f) markerApiV1MarkerPost markerResponseApiV1MarkerRequestIdGet markerFinalResponseStatus markerFinalResponseError

pollFinal :: forall reqI contentTypeI acceptI reqF contentTypeF resF acceptF.
  (Produces reqI acceptI, MimeUnrender acceptI InitialResponse, MimeType contentTypeI)
  => (Produces reqF acceptF, MimeUnrender acceptF resF, MimeType contentTypeF)
  => FastEnv -> File
  -> (File -> FastRequest reqI contentTypeI InitialResponse acceptI)
  -> (RequestId -> FastRequest reqF contentTypeF resF acceptF)
  -> (resF -> Text)
  -> (resF -> Maybe Text)
  -> IO (Maybe resF)
pollFinal (c, man) f reqI reqF statusCheck errorCheck = do
  print (reqI f)
  either onL onR =<< dispatchMime' man c (reqI f)
  where
    onR ir = handleInitialResponse ir >>= maybe (return Nothing) go
    onL e = putStrLn ("Initial Response Error: " ++ show e) >> return Nothing
    go (r, url) = do
      either onL' onR' =<< dispatchMime' man c (reqF r)
      where
        onL' e = putStrLn ("Final Response Error: " ++ show e) >> pure Nothing
        onR' fr = do
          -- Here we check if the response is ready by polling
          -- the endpoint every 2 seconds
          case statusCheck fr of
            "completed" -> do
              putStrLn "Response ready!"
              return $ Just fr
            e' -> do
              putStrLn "Response not ready yet..."
              putStrLn $ "Status: " ++ T.unpack e'
              case errorCheck fr of
                Nothing -> do
                  putStrLn "No error..."
                  putStrLn $ "Polling URL: " ++ show url
                  threadDelay 2000000
                  go (r, url)
                Just e -> do
                  putStrLn $ "Error in final response: " <> show e
                  return Nothing
