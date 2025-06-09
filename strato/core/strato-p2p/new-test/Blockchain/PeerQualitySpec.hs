{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Blockchain.PeerQualitySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Crypto.Types.PubKey.ECC (Point(..))
import Blockchain.PeerQuality

spec :: Spec
spec = describe "Blockchain.PeerQuality" $ do

  describe "calculatePeerScore" $ do
    it "always returns a score between 0.0 and 1.0" $ property $
      -- given
      \peerQuality ->
        -- when
        let score = calculatePeerScore peerQuality
        -- then
        in score >= 0.0 && score <= 1.0

    it "returns 0.5 for peers with no message statistics" $ property $
      -- given
      \peerQuality ->
        let peerQualityNoStats = peerQuality { pqMessageStats = Map.empty }
        -- when
            score = calculatePeerScore peerQualityNoStats
        in score == 0.5

  describe "messageTypeScore" $ do

    it "always returns a score between 0.0 and 1.0" $ property $
      -- given
      \msgType msgStats ->
        -- when
        let score = messageTypeScore msgType msgStats
        -- then
        in score >= 0.0 && score <= 1.0

--------------------------------------------------------------------------------

-- | Create a simple dummy Point for testing
dummyPoint :: Point
dummyPoint = Point 1 2

-- | Arbitrary instance for generating test PeerQuality data
instance Arbitrary PeerQuality where
  arbitrary = do
    now <- arbitrary :: Gen UTCTime
    messageStats <- arbitrary
    score <- choose (0.0, 1.0)
    return $ PeerQuality
      { pqPeerId = dummyPoint
      , pqConnectedAt = now
      , pqLastActivity = now
      , pqMessageStats = messageStats
      , pqOverallScore = score
      }

-- | Arbitrary instance for MessageType
instance Arbitrary MessageType where
  arbitrary = elements [P2PWireProtocol, Responses, HeaderRequest, BlockRequest,
                       PrivateChainRequest, TransactionRequest, BlockResponse, ConsensusMsg]

-- | Arbitrary instance for MessageStats
instance Arbitrary MessageStats where
  arbitrary = do
    count <- chooseInt (0, 1000)
    failures <- chooseInt (0, count)
    avgTime <- choose (0.0, 100000.0)
    lastSeen <- arbitrary
    pure $ MessageStats
      { msCount = count
      , msFailures = failures
      , msAvgResponseTime = avgTime
      , msLastSeen = lastSeen
      }
