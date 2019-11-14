-- file test/Main.hs
module Main where

import qualified System.Smartmon.ParserSpec   as P1
import qualified System.Smartmon.ParserSpec_2 as P2
import           Test.Hspec

main :: IO ()
main = hspec $ do
  P1.spec
  P2.spec

