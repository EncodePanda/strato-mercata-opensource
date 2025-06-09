module Main where

import Test.Hspec
import qualified Blockchain.PeerQualitySpec

main :: IO ()
main = hspec $ do
  describe "PeerQuality Tests" Blockchain.PeerQualitySpec.spec