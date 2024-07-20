{-
   FastAPI

   No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)

   OpenAPI Version: 3.1.0
   FastAPI API version: 0.1.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Fast.Model
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Fast.Model where

import Fast.Core
import Fast.MimeTypes

import Data.Aeson ((.:),(.:!),(.:?),(.=))

import qualified Control.Arrow as P (left)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as TI
import qualified Lens.Micro as L
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Control.Applicative ((<|>))
import Control.Applicative (Alternative)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import Prelude (($),(/=),(.),(<$>),(<*>),(>>=),(=<<),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)

import qualified Prelude as P



-- * Parameter newtypes


-- ** ExtractImages
newtype ExtractImages = ExtractImages { unExtractImages :: Bool } deriving (P.Eq, P.Show)

-- ** File
newtype File = File { unFile :: FilePath } deriving (P.Eq, P.Show)

-- ** ForceOcr
newtype ForceOcr = ForceOcr { unForceOcr :: Bool } deriving (P.Eq, P.Show)

-- ** Langs
newtype Langs = Langs { unLangs :: Text } deriving (P.Eq, P.Show)

-- ** MaxPages
newtype MaxPages = MaxPages { unMaxPages :: Int } deriving (P.Eq, P.Show)

-- ** Paginate
newtype Paginate = Paginate { unPaginate :: Bool } deriving (P.Eq, P.Show)

-- ** RequestId
newtype RequestId = RequestId { unRequestId :: Int } deriving (P.Eq, P.Show)

-- * Models


-- ** HTTPValidationError
-- | HTTPValidationError
-- HTTPValidationError
-- 
data HTTPValidationError = HTTPValidationError
  { hTTPValidationErrorDetail :: !(Maybe [ValidationError]) -- ^ "detail"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON HTTPValidationError
instance A.FromJSON HTTPValidationError where
  parseJSON = A.withObject "HTTPValidationError" $ \o ->
    HTTPValidationError
      <$> (o .:? "detail")

-- | ToJSON HTTPValidationError
instance A.ToJSON HTTPValidationError where
  toJSON HTTPValidationError {..} =
   _omitNulls
      [ "detail" .= hTTPValidationErrorDetail
      ]


-- | Construct a value of type 'HTTPValidationError' (by applying it's required fields, if any)
mkHTTPValidationError
  :: HTTPValidationError
mkHTTPValidationError =
  HTTPValidationError
  { hTTPValidationErrorDetail = Nothing
  }

-- ** HealthResponse
-- | HealthResponse
-- HealthResponse
-- 
data HealthResponse = HealthResponse
  { healthResponseStatus :: !(Text) -- ^ /Required/ "status" - The status of the service.  Should be &#39;ok&#39; if the service is running correctly.
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON HealthResponse
instance A.FromJSON HealthResponse where
  parseJSON = A.withObject "HealthResponse" $ \o ->
    HealthResponse
      <$> (o .:  "status")

-- | ToJSON HealthResponse
instance A.ToJSON HealthResponse where
  toJSON HealthResponse {..} =
   _omitNulls
      [ "status" .= healthResponseStatus
      ]


-- | Construct a value of type 'HealthResponse' (by applying it's required fields, if any)
mkHealthResponse
  :: Text -- ^ 'healthResponseStatus': The status of the service.  Should be 'ok' if the service is running correctly.
  -> HealthResponse
mkHealthResponse healthResponseStatus =
  HealthResponse
  { healthResponseStatus
  }

-- ** InitialResponse
-- | InitialResponse
-- InitialResponse
-- 
data InitialResponse = InitialResponse
  { initialResponseSuccess :: !(Maybe Bool) -- ^ "success" - Whether the request was successful.
  , initialResponseError :: !(Maybe Text) -- ^ "error"
  , initialResponseRequestId :: !(Int) -- ^ /Required/ "request_id" - The ID of the request. This ID can be used to check the status of the request.
  , initialResponseRequestCheckUrl :: !(Text) -- ^ /Required/ "request_check_url" - The URL to check the status of the request and get results.
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON InitialResponse
instance A.FromJSON InitialResponse where
  parseJSON = A.withObject "InitialResponse" $ \o ->
    InitialResponse
      <$> (o .:? "success")
      <*> (o .:? "error")
      <*> (o .:  "request_id")
      <*> (o .:  "request_check_url")

-- | ToJSON InitialResponse
instance A.ToJSON InitialResponse where
  toJSON InitialResponse {..} =
   _omitNulls
      [ "success" .= initialResponseSuccess
      , "error" .= initialResponseError
      , "request_id" .= initialResponseRequestId
      , "request_check_url" .= initialResponseRequestCheckUrl
      ]


-- | Construct a value of type 'InitialResponse' (by applying it's required fields, if any)
mkInitialResponse
  :: Int -- ^ 'initialResponseRequestId': The ID of the request. This ID can be used to check the status of the request.
  -> Text -- ^ 'initialResponseRequestCheckUrl': The URL to check the status of the request and get results.
  -> InitialResponse
mkInitialResponse initialResponseRequestId initialResponseRequestCheckUrl =
  InitialResponse
  { initialResponseSuccess = Nothing
  , initialResponseError = Nothing
  , initialResponseRequestId
  , initialResponseRequestCheckUrl
  }

-- ** LayoutFinalResponse
-- | LayoutFinalResponse
-- LayoutFinalResponse
-- 
data LayoutFinalResponse = LayoutFinalResponse
  { layoutFinalResponseStatus :: !(Text) -- ^ /Required/ "status" - The status of the request.  Should be &#39;complete&#39; when the request is done.
  , layoutFinalResponsePages :: !(Maybe [A.Value]) -- ^ "pages"
  , layoutFinalResponseSuccess :: !(Maybe Bool) -- ^ "success"
  , layoutFinalResponseError :: !(Maybe Text) -- ^ "error"
  , layoutFinalResponsePageCount :: !(Maybe Int) -- ^ "page_count"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON LayoutFinalResponse
instance A.FromJSON LayoutFinalResponse where
  parseJSON = A.withObject "LayoutFinalResponse" $ \o ->
    LayoutFinalResponse
      <$> (o .:  "status")
      <*> (o .:? "pages")
      <*> (o .:? "success")
      <*> (o .:? "error")
      <*> (o .:? "page_count")

-- | ToJSON LayoutFinalResponse
instance A.ToJSON LayoutFinalResponse where
  toJSON LayoutFinalResponse {..} =
   _omitNulls
      [ "status" .= layoutFinalResponseStatus
      , "pages" .= layoutFinalResponsePages
      , "success" .= layoutFinalResponseSuccess
      , "error" .= layoutFinalResponseError
      , "page_count" .= layoutFinalResponsePageCount
      ]


-- | Construct a value of type 'LayoutFinalResponse' (by applying it's required fields, if any)
mkLayoutFinalResponse
  :: Text -- ^ 'layoutFinalResponseStatus': The status of the request.  Should be 'complete' when the request is done.
  -> LayoutFinalResponse
mkLayoutFinalResponse layoutFinalResponseStatus =
  LayoutFinalResponse
  { layoutFinalResponseStatus
  , layoutFinalResponsePages = Nothing
  , layoutFinalResponseSuccess = Nothing
  , layoutFinalResponseError = Nothing
  , layoutFinalResponsePageCount = Nothing
  }

-- ** LineDetectionFinalResponse
-- | LineDetectionFinalResponse
-- LineDetectionFinalResponse
-- 
data LineDetectionFinalResponse = LineDetectionFinalResponse
  { lineDetectionFinalResponseStatus :: !(Text) -- ^ /Required/ "status" - The status of the request.  Should be &#39;complete&#39; when the request is done.
  , lineDetectionFinalResponsePages :: !(Maybe [A.Value]) -- ^ "pages"
  , lineDetectionFinalResponseSuccess :: !(Maybe Bool) -- ^ "success"
  , lineDetectionFinalResponseError :: !(Maybe Text) -- ^ "error"
  , lineDetectionFinalResponsePageCount :: !(Maybe Int) -- ^ "page_count"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON LineDetectionFinalResponse
instance A.FromJSON LineDetectionFinalResponse where
  parseJSON = A.withObject "LineDetectionFinalResponse" $ \o ->
    LineDetectionFinalResponse
      <$> (o .:  "status")
      <*> (o .:? "pages")
      <*> (o .:? "success")
      <*> (o .:? "error")
      <*> (o .:? "page_count")

-- | ToJSON LineDetectionFinalResponse
instance A.ToJSON LineDetectionFinalResponse where
  toJSON LineDetectionFinalResponse {..} =
   _omitNulls
      [ "status" .= lineDetectionFinalResponseStatus
      , "pages" .= lineDetectionFinalResponsePages
      , "success" .= lineDetectionFinalResponseSuccess
      , "error" .= lineDetectionFinalResponseError
      , "page_count" .= lineDetectionFinalResponsePageCount
      ]


-- | Construct a value of type 'LineDetectionFinalResponse' (by applying it's required fields, if any)
mkLineDetectionFinalResponse
  :: Text -- ^ 'lineDetectionFinalResponseStatus': The status of the request.  Should be 'complete' when the request is done.
  -> LineDetectionFinalResponse
mkLineDetectionFinalResponse lineDetectionFinalResponseStatus =
  LineDetectionFinalResponse
  { lineDetectionFinalResponseStatus
  , lineDetectionFinalResponsePages = Nothing
  , lineDetectionFinalResponseSuccess = Nothing
  , lineDetectionFinalResponseError = Nothing
  , lineDetectionFinalResponsePageCount = Nothing
  }

-- ** MarkerFinalResponse
-- | MarkerFinalResponse
-- MarkerFinalResponse
-- 
data MarkerFinalResponse = MarkerFinalResponse
  { markerFinalResponseStatus :: !(Text) -- ^ /Required/ "status" - The status of the request.  Should be &#39;complete&#39; when the request is done.
  , markerFinalResponseMarkdown :: !(Maybe Text) -- ^ "markdown"
  , markerFinalResponseImages :: !(Maybe (Map.Map String Text)) -- ^ "images"
  , markerFinalResponseMeta :: !(Maybe A.Value) -- ^ "meta"
  , markerFinalResponseSuccess :: !(Maybe Bool) -- ^ "success"
  , markerFinalResponseError :: !(Maybe Text) -- ^ "error"
  , markerFinalResponsePageCount :: !(Maybe Int) -- ^ "page_count"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON MarkerFinalResponse
instance A.FromJSON MarkerFinalResponse where
  parseJSON = A.withObject "MarkerFinalResponse" $ \o ->
    MarkerFinalResponse
      <$> (o .:  "status")
      <*> (o .:? "markdown")
      <*> (o .:? "images")
      <*> (o .:? "meta")
      <*> (o .:? "success")
      <*> (o .:? "error")
      <*> (o .:? "page_count")

-- | ToJSON MarkerFinalResponse
instance A.ToJSON MarkerFinalResponse where
  toJSON MarkerFinalResponse {..} =
   _omitNulls
      [ "status" .= markerFinalResponseStatus
      , "markdown" .= markerFinalResponseMarkdown
      , "images" .= markerFinalResponseImages
      , "meta" .= markerFinalResponseMeta
      , "success" .= markerFinalResponseSuccess
      , "error" .= markerFinalResponseError
      , "page_count" .= markerFinalResponsePageCount
      ]


-- | Construct a value of type 'MarkerFinalResponse' (by applying it's required fields, if any)
mkMarkerFinalResponse
  :: Text -- ^ 'markerFinalResponseStatus': The status of the request.  Should be 'complete' when the request is done.
  -> MarkerFinalResponse
mkMarkerFinalResponse markerFinalResponseStatus =
  MarkerFinalResponse
  { markerFinalResponseStatus
  , markerFinalResponseMarkdown = Nothing
  , markerFinalResponseImages = Nothing
  , markerFinalResponseMeta = Nothing
  , markerFinalResponseSuccess = Nothing
  , markerFinalResponseError = Nothing
  , markerFinalResponsePageCount = Nothing
  }

-- ** OCRFinalResponse
-- | OCRFinalResponse
-- OCRFinalResponse
-- 
data OCRFinalResponse = OCRFinalResponse
  { oCRFinalResponseStatus :: !(Text) -- ^ /Required/ "status" - The status of the request.  Should be &#39;complete&#39; when the request is done.
  , oCRFinalResponsePages :: !(Maybe [A.Value]) -- ^ "pages"
  , oCRFinalResponseSuccess :: !(Maybe Bool) -- ^ "success"
  , oCRFinalResponseError :: !(Maybe Text) -- ^ "error"
  , oCRFinalResponsePageCount :: !(Maybe Int) -- ^ "page_count"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON OCRFinalResponse
instance A.FromJSON OCRFinalResponse where
  parseJSON = A.withObject "OCRFinalResponse" $ \o ->
    OCRFinalResponse
      <$> (o .:  "status")
      <*> (o .:? "pages")
      <*> (o .:? "success")
      <*> (o .:? "error")
      <*> (o .:? "page_count")

-- | ToJSON OCRFinalResponse
instance A.ToJSON OCRFinalResponse where
  toJSON OCRFinalResponse {..} =
   _omitNulls
      [ "status" .= oCRFinalResponseStatus
      , "pages" .= oCRFinalResponsePages
      , "success" .= oCRFinalResponseSuccess
      , "error" .= oCRFinalResponseError
      , "page_count" .= oCRFinalResponsePageCount
      ]


-- | Construct a value of type 'OCRFinalResponse' (by applying it's required fields, if any)
mkOCRFinalResponse
  :: Text -- ^ 'oCRFinalResponseStatus': The status of the request.  Should be 'complete' when the request is done.
  -> OCRFinalResponse
mkOCRFinalResponse oCRFinalResponseStatus =
  OCRFinalResponse
  { oCRFinalResponseStatus
  , oCRFinalResponsePages = Nothing
  , oCRFinalResponseSuccess = Nothing
  , oCRFinalResponseError = Nothing
  , oCRFinalResponsePageCount = Nothing
  }

-- ** ValidationError
-- | ValidationError
-- ValidationError
-- 
data ValidationError = ValidationError
  { validationErrorLoc :: !([ValidationErrorLocInner]) -- ^ /Required/ "loc"
  , validationErrorMsg :: !(Text) -- ^ /Required/ "msg"
  , validationErrorType :: !(Text) -- ^ /Required/ "type"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ValidationError
instance A.FromJSON ValidationError where
  parseJSON = A.withObject "ValidationError" $ \o ->
    ValidationError
      <$> (o .:  "loc")
      <*> (o .:  "msg")
      <*> (o .:  "type")

-- | ToJSON ValidationError
instance A.ToJSON ValidationError where
  toJSON ValidationError {..} =
   _omitNulls
      [ "loc" .= validationErrorLoc
      , "msg" .= validationErrorMsg
      , "type" .= validationErrorType
      ]


-- | Construct a value of type 'ValidationError' (by applying it's required fields, if any)
mkValidationError
  :: [ValidationErrorLocInner] -- ^ 'validationErrorLoc' 
  -> Text -- ^ 'validationErrorMsg' 
  -> Text -- ^ 'validationErrorType' 
  -> ValidationError
mkValidationError validationErrorLoc validationErrorMsg validationErrorType =
  ValidationError
  { validationErrorLoc
  , validationErrorMsg
  , validationErrorType
  }

-- ** ValidationErrorLocInner
-- | ValidationErrorLocInner
data ValidationErrorLocInner = ValidationErrorLocInner
  { 
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ValidationErrorLocInner
instance A.FromJSON ValidationErrorLocInner where
  parseJSON = A.withObject "ValidationErrorLocInner" $ \o ->
    pure ValidationErrorLocInner
      

-- | ToJSON ValidationErrorLocInner
instance A.ToJSON ValidationErrorLocInner where
  toJSON ValidationErrorLocInner  =
   _omitNulls
      [ 
      ]


-- | Construct a value of type 'ValidationErrorLocInner' (by applying it's required fields, if any)
mkValidationErrorLocInner
  :: ValidationErrorLocInner
mkValidationErrorLocInner =
  ValidationErrorLocInner
  { 
  }




-- * Auth Methods

-- ** AuthApiKeyAPIKeyHeader
data AuthApiKeyAPIKeyHeader =
  AuthApiKeyAPIKeyHeader Text -- ^ secret
  deriving (P.Eq, P.Show, P.Typeable)

instance AuthMethod AuthApiKeyAPIKeyHeader where
  applyAuthMethod _ a@(AuthApiKeyAPIKeyHeader secret) req =
    P.pure $
    if (P.typeOf a `P.elem` rAuthTypes req)
      then req `setHeader` toHeader ("X-API-Key", secret)
           & L.over rAuthTypesL (P.filter (/= P.typeOf a))
      else req


