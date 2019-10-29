{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module System.Smartmon.Parser
  ( getSmartInfo
  , parseFile
  , parseSmart
  , SmartInfo(..)
  , SmartValue(..)
  ) where

import           System.Smartmon.Datatypes

import           Data.Aeson                      (Value (..), decode)
import           Data.Aeson.AutoType.Alternative
import qualified Data.ByteString.Lazy.Char8      as BSL
import qualified Data.Text                       as T (Text)
import           System.Exit                     (exitFailure)
import           System.IO                       (hPutStrLn, stderr)

data SmartInfo = SmartInfo
  { _smPowOnTime  :: SmartValue Int,
    _smDriveModel :: SmartValue T.Text,
    _smRotRate    :: SmartValue Int
  } deriving (Eq, Show)

data SmartValue a = Unknown
                  | DecodeError
                  | SmartValue a
                  deriving (Eq, Show)

fetchBy :: (a -> b) -> Maybe (a :|: [Maybe Value]) -> SmartValue b
fetchBy _ Nothing             = Unknown
fetchBy f (Just (AltLeft x))  = SmartValue $ f x
fetchBy _ (Just (AltRight _)) = DecodeError

getSmartInfo :: Smart -> SmartInfo
getSmartInfo sd = SmartInfo {
  _smPowOnTime  = fetchBy (round . powerOnTimeHours) . topLevelPowerOnTime $ sd,
  _smDriveModel = fetchBy id . topLevelModelName $ sd,
  _smRotRate    = fetchBy round . topLevelRotationRate $ sd
  }

parseSmart :: BSL.ByteString -> Maybe Smart
parseSmart = decode

parseFile ∷ FilePath → IO Smart
parseFile filename = do
  input <- BSL.readFile filename
  case decode input of
    Nothing -> fatal $ case (decode input :: Maybe Value) of
                         Nothing -> "Invalid JSON file: " <> filename
                         Just _  -> "Mismatched JSON value from file: " <> filename
    Just r  -> return (r :: Smart)
  where
    fatal ∷ String → IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

{-
main ∷ IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess
-}
