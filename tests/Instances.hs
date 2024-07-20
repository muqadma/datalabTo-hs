{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import Fast.Model
import Fast.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary HTTPValidationError where
  arbitrary = sized genHTTPValidationError

genHTTPValidationError :: Int -> Gen HTTPValidationError
genHTTPValidationError n =
  HTTPValidationError
    <$> arbitraryReducedMaybe n -- hTTPValidationErrorDetail :: Maybe [ValidationError]
  
instance Arbitrary HealthResponse where
  arbitrary = sized genHealthResponse

genHealthResponse :: Int -> Gen HealthResponse
genHealthResponse n =
  HealthResponse
    <$> arbitrary -- healthResponseStatus :: Text
  
instance Arbitrary InitialResponse where
  arbitrary = sized genInitialResponse

genInitialResponse :: Int -> Gen InitialResponse
genInitialResponse n =
  InitialResponse
    <$> arbitraryReducedMaybe n -- initialResponseSuccess :: Maybe Bool
    <*> arbitraryReducedMaybe n -- initialResponseError :: Maybe Text
    <*> arbitrary -- initialResponseRequestId :: Int
    <*> arbitrary -- initialResponseRequestCheckUrl :: Text
  
instance Arbitrary LayoutFinalResponse where
  arbitrary = sized genLayoutFinalResponse

genLayoutFinalResponse :: Int -> Gen LayoutFinalResponse
genLayoutFinalResponse n =
  LayoutFinalResponse
    <$> arbitrary -- layoutFinalResponseStatus :: Text
    <*> arbitraryReducedMaybe n -- layoutFinalResponsePages :: Maybe [A.Value]
    <*> arbitraryReducedMaybe n -- layoutFinalResponseSuccess :: Maybe Bool
    <*> arbitraryReducedMaybe n -- layoutFinalResponseError :: Maybe Text
    <*> arbitraryReducedMaybe n -- layoutFinalResponsePageCount :: Maybe Int
  
instance Arbitrary LineDetectionFinalResponse where
  arbitrary = sized genLineDetectionFinalResponse

genLineDetectionFinalResponse :: Int -> Gen LineDetectionFinalResponse
genLineDetectionFinalResponse n =
  LineDetectionFinalResponse
    <$> arbitrary -- lineDetectionFinalResponseStatus :: Text
    <*> arbitraryReducedMaybe n -- lineDetectionFinalResponsePages :: Maybe [A.Value]
    <*> arbitraryReducedMaybe n -- lineDetectionFinalResponseSuccess :: Maybe Bool
    <*> arbitraryReducedMaybe n -- lineDetectionFinalResponseError :: Maybe Text
    <*> arbitraryReducedMaybe n -- lineDetectionFinalResponsePageCount :: Maybe Int
  
instance Arbitrary MarkerFinalResponse where
  arbitrary = sized genMarkerFinalResponse

genMarkerFinalResponse :: Int -> Gen MarkerFinalResponse
genMarkerFinalResponse n =
  MarkerFinalResponse
    <$> arbitrary -- markerFinalResponseStatus :: Text
    <*> arbitraryReducedMaybe n -- markerFinalResponseMarkdown :: Maybe Text
    <*> arbitraryReducedMaybe n -- markerFinalResponseImages :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybeValue n -- markerFinalResponseMeta :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- markerFinalResponseSuccess :: Maybe Bool
    <*> arbitraryReducedMaybe n -- markerFinalResponseError :: Maybe Text
    <*> arbitraryReducedMaybe n -- markerFinalResponsePageCount :: Maybe Int
  
instance Arbitrary OCRFinalResponse where
  arbitrary = sized genOCRFinalResponse

genOCRFinalResponse :: Int -> Gen OCRFinalResponse
genOCRFinalResponse n =
  OCRFinalResponse
    <$> arbitrary -- oCRFinalResponseStatus :: Text
    <*> arbitraryReducedMaybe n -- oCRFinalResponsePages :: Maybe [A.Value]
    <*> arbitraryReducedMaybe n -- oCRFinalResponseSuccess :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oCRFinalResponseError :: Maybe Text
    <*> arbitraryReducedMaybe n -- oCRFinalResponsePageCount :: Maybe Int
  
instance Arbitrary ValidationError where
  arbitrary = sized genValidationError

genValidationError :: Int -> Gen ValidationError
genValidationError n =
  ValidationError
    <$> arbitraryReduced n -- validationErrorLoc :: [ValidationErrorLocInner]
    <*> arbitrary -- validationErrorMsg :: Text
    <*> arbitrary -- validationErrorType :: Text
  
instance Arbitrary ValidationErrorLocInner where
  arbitrary = sized genValidationErrorLocInner

genValidationErrorLocInner :: Int -> Gen ValidationErrorLocInner
genValidationErrorLocInner n =
  
  pure ValidationErrorLocInner
   



