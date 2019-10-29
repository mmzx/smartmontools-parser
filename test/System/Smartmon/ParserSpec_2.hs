{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module System.Smartmon.ParserSpec_2
  ( spec
  ) where

import           Test.Hspec
import           System.Smartmon.Parser

spec :: Spec
spec = do
  describe "Fetching relevant data of " $ do
    it "failed smart reading." $ do
      let expected = SmartInfo Unknown Unknown Unknown
      f <- parseFile "samples/smartInfo_fail-1.json"
      let result = getSmartInfo f
      result `shouldBe` expected

    it "Intel rapid storage RAID array." $ do
      let expected = SmartInfo Unknown (SmartValue "Intel Raid 1 Volume") (SmartValue 7200)
      f <- parseFile "samples/smartInfo_raid.json"
      let result = getSmartInfo f
      result `shouldBe` expected

    it "Optical drive." $ do
      let expected = SmartInfo Unknown (SmartValue "HL-DT-ST DVDRAM GH24NSC0") Unknown
      f <- parseFile "samples/smartInfo_optical.json"
      let result = getSmartInfo f
      result `shouldBe` expected

    it "Spinning disk (HDD)." $ do
      let expected = SmartInfo (SmartValue 846) (SmartValue "X SSD 850 PRO 128GB") (SmartValue 0)
      f <- parseFile "samples/smartInfo_sda.json"
      let result = getSmartInfo f
      result `shouldBe` expected
