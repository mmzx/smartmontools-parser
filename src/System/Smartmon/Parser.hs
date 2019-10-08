{-# LANGUAGE UnicodeSyntax #-}
module System.Smartmon.Parser
  ( parseFile
  ) where

import           System.Smartmon.Datatypes

import           Data.Aeson                 (Value (..), decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Exit                (exitFailure)
import           System.IO                  (hPutStrLn, stderr)

parseFile ∷ FilePath → IO Smart
parseFile filename = do
  input <- BSL.readFile filename
  case decode input of
    Nothing -> fatal $ case (decode input :: Maybe Value) of
                         Nothing -> "Invalid JSON file: " ++ filename
                         Just _  -> "Mismatched JSON value from file: " ++ filename
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
