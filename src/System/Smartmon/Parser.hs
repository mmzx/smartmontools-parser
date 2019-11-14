{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module System.Smartmon.Parser
  ( getSmartInfo
  , parseFile
  , parseSmart
  , mkSmartInfo
  , smPowOnTime
  , smDriveModel
  , smRotRate
  , smDrvSerial
  , SmartInfo(..)
  , SmartValue(..)
  ) where

import           System.Smartmon.Datatypes

import           Control.Lens
import           Data.Aeson                      (Value (..), decode)
import           Data.Aeson.AutoType.Alternative
import qualified Data.ByteString.Lazy.Char8      as BSL
import qualified Data.Text                       as T (Text)

data SmartInfo = SmartInfo
  { _smPowOnTime  :: SmartValue Int,
    _smDriveModel :: SmartValue T.Text,
    _smRotRate    :: SmartValue Int,
    _smDrvSerial  :: SmartValue T.Text
  } deriving (Eq)

data SmartValue a = Unknown
                  | DecodeError
                  | SmartValue a
                  deriving (Eq)

instance (Ord a,Num a) => Num (SmartValue a) where

  (+) (SmartValue x) (SmartValue y) = SmartValue $ x + y
  (+) _ _                           = Unknown

  (*) (SmartValue x) (SmartValue y) = SmartValue $ x * y
  (*) _ _                           = Unknown

  (-) (SmartValue x) (SmartValue y) = SmartValue $ x - y
  (-) _ _                           = Unknown

  abs (SmartValue x) = SmartValue $ abs x
  abs _              = Unknown

  fromInteger = SmartValue . fromInteger

  signum (SmartValue x)
    | x < 0     = SmartValue (-1)
    | x == 0    = SmartValue 0
    | otherwise = SmartValue 1
  signum _ = Unknown

makeLenses ''SmartInfo

mkSmartInfo :: SmartInfo
mkSmartInfo = SmartInfo Unknown Unknown Unknown Unknown

fetchBy :: (a -> b) -> Maybe (a :|: [Maybe Value]) -> SmartValue b
fetchBy _ Nothing             = Unknown
fetchBy f (Just (AltLeft x))  = SmartValue $ f x
fetchBy _ (Just (AltRight _)) = DecodeError

getSmartInfo :: Smart -> SmartInfo
getSmartInfo sd = SmartInfo {
  _smPowOnTime  = fetchBy (round . powerOnTimeHours) . topLevelPowerOnTime $ sd,
  _smDriveModel = fetchBy id . topLevelModelName $ sd,
  _smRotRate    = fetchBy round . topLevelRotationRate $ sd,
  _smDrvSerial  = fetchBy id . topLevelSerialNumber $ sd
  }

parseSmart :: BSL.ByteString -> Maybe Smart
parseSmart = decode

parseFile :: FilePath -> IO (Maybe Smart)
parseFile filename = BSL.readFile filename >>= return . parseSmart

{-
main ∷ IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess
-}
