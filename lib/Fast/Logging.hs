{-
   FastAPI

   No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)

   OpenAPI Version: 3.1.0
   FastAPI API version: 0.1.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Fast.Logging
Logging functions
-}
{-# LANGUAGE CPP #-}

#ifdef USE_KATIP

module Fast.Logging
  ( module Fast.LoggingKatip
  ) where

import Fast.LoggingKatip

#else

module Fast.Logging
  ( module Fast.LoggingMonadLogger
  ) where

import Fast.LoggingMonadLogger

#endif
