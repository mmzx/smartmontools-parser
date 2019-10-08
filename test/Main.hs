-- file test/Main.hs
module Main where

import qualified System.Smartmon.ParserSpec as P

import           Test.Hspec

main :: IO ()
main = hspec $ do
  P.spec

