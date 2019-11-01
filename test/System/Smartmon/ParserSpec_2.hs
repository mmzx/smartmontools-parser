{-# LANGUAGE OverloadedStrings #-}

module System.Smartmon.ParserSpec_2
  ( spec
  ) where

import           System.Smartmon.Parser

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8      as BSL (readFile)
import           Test.Hspec
import qualified Test.QuickCheck as Q

spec :: Spec
spec = do
  describe "Fetching relevant data of " $ do

    it "When the tool cannot read smart data." $ do
      f <- parseFile "samples/smartInfo_fail-1.json"
      let result = getSmartInfo <$> f
          expected = Just mkSmartInfo
      result `shouldBe` expected

    it "Parsing fails." $ do
      f <- parseFile "samples/incorrect-1.json"
      let result = getSmartInfo <$> f
          expected = Nothing
      result `shouldBe` expected

    it "Intel rapid storage RAID array." $ do
      let expected = Just $  set smRotRate 7200
                     . set smDriveModel (SmartValue "Intel Raid 1 Volume")
                     . set smPowOnTime Unknown $ mkSmartInfo
      f <- BSL.readFile "samples/smartInfo_raid.json"
      let result = getSmartInfo <$> parseSmart f
      result `shouldBe` expected

    it "Optical drive." $ do
      let expected = Just $ SmartInfo Unknown (SmartValue "HL-DT-ST DVDRAM GH24NSC0") Unknown
      f <- parseFile "samples/smartInfo_optical.json"
      let result = getSmartInfo <$> f
      result `shouldBe` expected

    it "Spinning disk (HDD)." $ do
      let expected = Just $ SmartInfo (SmartValue 846) (SmartValue "X SSD 850 PRO 128GB") (SmartValue 0)
      f <- parseFile "samples/smartInfo_sda.json"
      let result = getSmartInfo <$> f
      result `shouldBe` expected

  describe "Num instance of SmartValue." $ do

    it "Operate on undecidable values 1." $ do
      let input = ((SmartValue 4) * DecodeError - (SmartValue 19)) :: SmartValue Int
          expected = Unknown
          result = signum input
      result `shouldBe` expected

    it "Operate on undecidable values 2." $ do
      let input = ((SmartValue 4) * Unknown - (SmartValue 19)) :: SmartValue Int
          expected = Unknown
          result = signum input
      result `shouldBe` expected

    it "Addition" $ Q.property $
       \x -> (SmartValue x + SmartValue x == SmartValue (x + (x::Int)))

    it "Multiplication" $ Q.property $
       \x -> (SmartValue x * SmartValue x == SmartValue (x * (x::Int)))

    it "Subtraction" $ Q.property $
       \x -> (SmartValue x - SmartValue x == SmartValue (x - (x::Int)))

    it "Abs" $ Q.property $
      \x -> (abs $ SmartValue x) == (SmartValue $ abs (x::Int))

    it "fromInteger" $ Q.property $
      \x -> fromInteger x == SmartValue x

    it "signum" $ do
      let input = [SmartValue (-7), SmartValue 7, SmartValue 0]  :: [SmartValue Int]
          expected = [(-1), 1, 0]
          result = map signum input
      result `shouldBe` expected

