{-
   FastAPI

   No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)

   OpenAPI Version: 3.1.0
   FastAPI API version: 0.1.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Fast.API.Api
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Fast.API.Api where

import Fast.Core
import Fast.MimeTypes
import Fast.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Api

-- *** healthApiV1HealthGet

-- | @GET \/api\/v1\/health@
-- 
-- Health
-- 
-- This endpoint is used to check the health of the API.  Returns a JSON object with the key \"status\" set to \"ok\".
-- 
healthApiV1HealthGet
  :: FastRequest HealthApiV1HealthGet MimeNoContent HealthResponse MimeJSON
healthApiV1HealthGet =
  _mkRequest "GET" ["/api/v1/health"]

data HealthApiV1HealthGet  
-- | @application/json@
instance Produces HealthApiV1HealthGet MimeJSON


-- *** layoutApiV1LayoutPost

-- | @POST \/api\/v1\/layout@
-- 
-- Layout
-- 
-- This endpoint is used to submit a PDF or image for layout and reading order detection.  The detected layout bboxes on the page will be returned, along with their predicted reading order and layout labels.  More information about the return data and labels is here - https://github.com/VikParuchuri/surya?tab=readme-ov-file#layout-analysis .
-- 
-- AuthMethod: 'AuthApiKeyAPIKeyHeader'
-- 
layoutApiV1LayoutPost
  :: (Consumes LayoutApiV1LayoutPost MimeMultipartFormData)
  => File -- ^ "file" -  Input PDF, word document, powerpoint, or image file, uploaded as multipart form data.  Images must be png, jpg, or webp format.
  -> FastRequest LayoutApiV1LayoutPost MimeMultipartFormData InitialResponse MimeJSON
layoutApiV1LayoutPost (File file) =
  _mkRequest "POST" ["/api/v1/layout"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAPIKeyHeader)
    `_addMultiFormPart` NH.partFileSource "file" file

data LayoutApiV1LayoutPost  
instance HasOptionalParam LayoutApiV1LayoutPost MaxPages where
  applyOptionalParam req (MaxPages xs) =
    req `_addMultiFormPart` NH.partLBS "max_pages" (mimeRender' MimeMultipartFormData xs)

-- | @multipart/form-data@
instance Consumes LayoutApiV1LayoutPost MimeMultipartFormData

-- | @application/json@
instance Produces LayoutApiV1LayoutPost MimeJSON


-- *** layoutResponseApiV1LayoutRequestIdGet

-- | @GET \/api\/v1\/layout\/{request_id}@
-- 
-- Layout Response
-- 
-- AuthMethod: 'AuthApiKeyAPIKeyHeader'
-- 
layoutResponseApiV1LayoutRequestIdGet
  :: RequestId -- ^ "requestId"
  -> FastRequest LayoutResponseApiV1LayoutRequestIdGet MimeNoContent LayoutFinalResponse MimeJSON
layoutResponseApiV1LayoutRequestIdGet (RequestId requestId) =
  _mkRequest "GET" ["/api/v1/layout/",toPath requestId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAPIKeyHeader)

data LayoutResponseApiV1LayoutRequestIdGet  
-- | @application/json@
instance Produces LayoutResponseApiV1LayoutRequestIdGet MimeJSON


-- *** lineDetectionApiV1LineDetectionPost

-- | @POST \/api\/v1\/line_detection@
-- 
-- Line Detection
-- 
-- This endpoint is used to submit a PDF or image for line detection.  The detected text lines on the page will be returned, along with their bbox and polygon coordinates.
-- 
-- AuthMethod: 'AuthApiKeyAPIKeyHeader'
-- 
lineDetectionApiV1LineDetectionPost
  :: (Consumes LineDetectionApiV1LineDetectionPost MimeMultipartFormData)
  => File -- ^ "file" -  Input PDF, word document, powerpoint, or image file, uploaded as multipart form data.  Images must be png, jpg, or webp format.
  -> FastRequest LineDetectionApiV1LineDetectionPost MimeMultipartFormData InitialResponse MimeJSON
lineDetectionApiV1LineDetectionPost (File file) =
  _mkRequest "POST" ["/api/v1/line_detection"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAPIKeyHeader)
    `_addMultiFormPart` NH.partFileSource "file" file

data LineDetectionApiV1LineDetectionPost  
instance HasOptionalParam LineDetectionApiV1LineDetectionPost MaxPages where
  applyOptionalParam req (MaxPages xs) =
    req `_addMultiFormPart` NH.partLBS "max_pages" (mimeRender' MimeMultipartFormData xs)

-- | @multipart/form-data@
instance Consumes LineDetectionApiV1LineDetectionPost MimeMultipartFormData

-- | @application/json@
instance Produces LineDetectionApiV1LineDetectionPost MimeJSON


-- *** lineDetectionResponseApiV1LineDetectionRequestIdGet

-- | @GET \/api\/v1\/line_detection\/{request_id}@
-- 
-- Line Detection Response
-- 
-- AuthMethod: 'AuthApiKeyAPIKeyHeader'
-- 
lineDetectionResponseApiV1LineDetectionRequestIdGet
  :: RequestId -- ^ "requestId"
  -> FastRequest LineDetectionResponseApiV1LineDetectionRequestIdGet MimeNoContent LineDetectionFinalResponse MimeJSON
lineDetectionResponseApiV1LineDetectionRequestIdGet (RequestId requestId) =
  _mkRequest "GET" ["/api/v1/line_detection/",toPath requestId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAPIKeyHeader)

data LineDetectionResponseApiV1LineDetectionRequestIdGet  
-- | @application/json@
instance Produces LineDetectionResponseApiV1LineDetectionRequestIdGet MimeJSON


-- *** markerApiV1MarkerPost

-- | @POST \/api\/v1\/marker@
-- 
-- Marker
-- 
-- This endpoint is used to submit a PDF file for conversion to markdown.  A PDF file must be uploaded, and the X-API-Key header must be set to the team's API key. file is the PDF file to be converted to markdown. Returns the converted markdown file and base64 encoded image files.
-- 
-- AuthMethod: 'AuthApiKeyAPIKeyHeader'
-- 
markerApiV1MarkerPost
  :: (Consumes MarkerApiV1MarkerPost MimeMultipartFormData)
  => File -- ^ "file" -  Input PDF, word document, or powerpoint, uploaded as multipart form data.
  -> FastRequest MarkerApiV1MarkerPost MimeMultipartFormData InitialResponse MimeJSON
markerApiV1MarkerPost (File file) =
  _mkRequest "POST" ["/api/v1/marker"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAPIKeyHeader)
    `_addMultiFormPart` NH.partFileSource "file" file

data MarkerApiV1MarkerPost  
instance HasOptionalParam MarkerApiV1MarkerPost MaxPages where
  applyOptionalParam req (MaxPages xs) =
    req `_addMultiFormPart` NH.partLBS "max_pages" (mimeRender' MimeMultipartFormData xs)
instance HasOptionalParam MarkerApiV1MarkerPost Langs where
  applyOptionalParam req (Langs xs) =
    req `_addMultiFormPart` NH.partLBS "langs" (mimeRender' MimeMultipartFormData xs)

-- | /Optional Param/ "force_ocr" - Force OCR on all pages of the PDF.  Defaults to False.  This can lead to worse results if you have good text in your PDFs (which is true in most cases).
instance HasOptionalParam MarkerApiV1MarkerPost ForceOcr where
  applyOptionalParam req (ForceOcr xs) =
    req `_addMultiFormPart` NH.partLBS "force_ocr" (mimeRender' MimeMultipartFormData xs)

-- | /Optional Param/ "paginate" - Whether to paginate the output.  Defaults to False.  If set to True, each page of the output will be separated by a horizontal rule (2 newlines, 16 - characters, 2 newlines).
instance HasOptionalParam MarkerApiV1MarkerPost Paginate where
  applyOptionalParam req (Paginate xs) =
    req `_addMultiFormPart` NH.partLBS "paginate" (mimeRender' MimeMultipartFormData xs)

-- | /Optional Param/ "extract_images" - Whether to extract images from the PDF.  Defaults to True.  If set to False, no images will be extracted from the PDF.
instance HasOptionalParam MarkerApiV1MarkerPost ExtractImages where
  applyOptionalParam req (ExtractImages xs) =
    req `_addMultiFormPart` NH.partLBS "extract_images" (mimeRender' MimeMultipartFormData xs)

-- | @multipart/form-data@
instance Consumes MarkerApiV1MarkerPost MimeMultipartFormData

-- | @application/json@
instance Produces MarkerApiV1MarkerPost MimeJSON


-- *** markerResponseApiV1MarkerRequestIdGet

-- | @GET \/api\/v1\/marker\/{request_id}@
-- 
-- Marker Response
-- 
-- AuthMethod: 'AuthApiKeyAPIKeyHeader'
-- 
markerResponseApiV1MarkerRequestIdGet
  :: RequestId -- ^ "requestId"
  -> FastRequest MarkerResponseApiV1MarkerRequestIdGet MimeNoContent MarkerFinalResponse MimeJSON
markerResponseApiV1MarkerRequestIdGet (RequestId requestId) =
  _mkRequest "GET" ["/api/v1/marker/",toPath requestId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAPIKeyHeader)

data MarkerResponseApiV1MarkerRequestIdGet  
-- | @application/json@
instance Produces MarkerResponseApiV1MarkerRequestIdGet MimeJSON


-- *** ocrApiV1OcrPost

-- | @POST \/api\/v1\/ocr@
-- 
-- Ocr
-- 
-- This endpoint is used to submit a PDF or image for OCR.  The OCR text lines will be returned, along with their bbox and polygon coordinates.  You can submit up to 4 languages used in the document for OCR.
-- 
-- AuthMethod: 'AuthApiKeyAPIKeyHeader'
-- 
ocrApiV1OcrPost
  :: (Consumes OcrApiV1OcrPost MimeMultipartFormData)
  => File -- ^ "file" -  Input PDF, word document, powerpoint, or image file, uploaded as multipart form data.  Images must be png, jpg, or webp format.
  -> FastRequest OcrApiV1OcrPost MimeMultipartFormData InitialResponse MimeJSON
ocrApiV1OcrPost (File file) =
  _mkRequest "POST" ["/api/v1/ocr"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAPIKeyHeader)
    `_addMultiFormPart` NH.partFileSource "file" file

data OcrApiV1OcrPost  
instance HasOptionalParam OcrApiV1OcrPost MaxPages where
  applyOptionalParam req (MaxPages xs) =
    req `_addMultiFormPart` NH.partLBS "max_pages" (mimeRender' MimeMultipartFormData xs)
instance HasOptionalParam OcrApiV1OcrPost Langs where
  applyOptionalParam req (Langs xs) =
    req `_addMultiFormPart` NH.partLBS "langs" (mimeRender' MimeMultipartFormData xs)

-- | @multipart/form-data@
instance Consumes OcrApiV1OcrPost MimeMultipartFormData

-- | @application/json@
instance Produces OcrApiV1OcrPost MimeJSON


-- *** ocrResponseApiV1OcrRequestIdGet

-- | @GET \/api\/v1\/ocr\/{request_id}@
-- 
-- Ocr Response
-- 
-- AuthMethod: 'AuthApiKeyAPIKeyHeader'
-- 
ocrResponseApiV1OcrRequestIdGet
  :: RequestId -- ^ "requestId"
  -> FastRequest OcrResponseApiV1OcrRequestIdGet MimeNoContent OCRFinalResponse MimeJSON
ocrResponseApiV1OcrRequestIdGet (RequestId requestId) =
  _mkRequest "GET" ["/api/v1/ocr/",toPath requestId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAPIKeyHeader)

data OcrResponseApiV1OcrRequestIdGet  
-- | @application/json@
instance Produces OcrResponseApiV1OcrRequestIdGet MimeJSON
