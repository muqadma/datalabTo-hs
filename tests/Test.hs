{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import Fast.Model
import Fast.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy HTTPValidationError)
      propMimeEq MimeJSON (Proxy :: Proxy HealthResponse)
      propMimeEq MimeJSON (Proxy :: Proxy InitialResponse)
      propMimeEq MimeJSON (Proxy :: Proxy LayoutFinalResponse)
      propMimeEq MimeJSON (Proxy :: Proxy LineDetectionFinalResponse)
      propMimeEq MimeJSON (Proxy :: Proxy MarkerFinalResponse)
      propMimeEq MimeJSON (Proxy :: Proxy OCRFinalResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ValidationError)
      propMimeEq MimeJSON (Proxy :: Proxy ValidationErrorLocInner)
      
