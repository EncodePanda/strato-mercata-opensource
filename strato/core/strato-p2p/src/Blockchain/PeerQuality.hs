{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Blockchain.PeerQuality (
   -- * Data Types
   PeerQuality (..)
 , MessageType (..)
 , MessageStats (..)

   -- * Scoring Functions
 , messageTypeScore

   -- * Message Classification
 , classifyMessage
 , expectedResponseTime

   -- * Utility Functions
 , updateMessageStats
 , emptyPeerQuality
 , emptyMessageStats
) where

import Crypto.Types.PubKey.ECC (Point)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Blockchain.Data.Wire (Message (..))
import qualified Data.Map.Strict as Map

-- | Complete quality assessment for a peer.
data PeerQuality = PeerQuality
  { pqPeerId       :: !Point
  -- ^ Peer's public key
  , pqConnectedAt  :: !UTCTime
  -- ^ When connection was established
  , pqLastActivity :: !UTCTime
  -- ^ Last message activity
  , pqMessageStats :: !(Map.Map MessageType MessageStats)
  -- ^ Per-message-type statistics
  , pqOverallScore :: !Double
  -- ^ Periodically calculated quality score (0.0-1.0)
  } deriving (Show, Eq, Generic)

-- | Classification of messages for performance tracking. We expect different
-- type of messages to return with different latency, because the size of the
-- message differs.
data MessageType
  = P2PWireProtocol
  -- ^ Small messages that require low latency
  | Responses
  -- ^ Most of the responses
  | HeaderRequest
  -- ^ All headers requests
  | BlockRequest
  -- ^ All blocks requests
  | PrivateChainRequest
  -- ^ All requests from private chains protocol
  | TransactionRequest
  -- ^ Transaction requests
  | BlockResponse
  -- ^ All block responses
  | ConsensusMsg
  -- ^ All Blockstanbul consensus messages
  deriving (Show, Eq, Ord, Generic)


-- | Statistics for a specific message type. We track each MessageStats per
-- MessageType
data MessageStats = MessageStats
  { msCount           :: !Int
  -- ^ Total number of messages of this type
  , msFailures        :: !Int
  -- ^ Number of failed requests/responses
  , msAvgResponseTime :: !Double
  -- ^ Average response time in milliseconds
  , msLastSeen        :: !UTCTime
  -- ^ Timestamp of last message of this type
  } deriving (Show, Eq, Generic)

newtype ReliabilityWeight = ReliabilityWeight Double

newtype LatencyWeight = LatencyWeight Double

-- | Response time thresholds for evaluating peer performance.
--
-- This data type defines three performance tiers for measuring how quickly
-- peers respond to different types of messages. The thresholds are used by
-- the scoring algorithm to evaluate peer quality.
--
-- Performance tiers:
-- * @_ideal@: Response times at or below this threshold receive perfect scores (1.0)
-- * @_reasonable@: Response times between ideal and reasonable receive good scores (0.5-1.0)
-- * @_fatal@: Response times between reasonable and fatal receive poor scores (0.0-0.5)
-- * Above @_fatal@: Response times above this threshold receive zero scores (0.0)
--
-- All values are in milliseconds.
data ExpectedResponseTime = ExpectedResponseTime
  { _ideal :: !Double
  -- ^ Average responses smaller or equal to this value are considered ideal
  , _reasonable :: !Double
  -- ^ Average responses between 'ideal' and this value are considered reasonable
  , _fatal :: !Double
  -- ^ Average responses between 'reasonable' and this value are considered
  -- problematic, averages bigger than this value are considred fatal
  }

-- | Calculate performance score for a specific message type
--
-- The score is a floating-point value between 0.0 (worst) and 1.0 (best)
-- that reflects how well a peer performs for a given message type
messageTypeScore :: MessageType -> MessageStats -> Double
messageTypeScore msgType MessageStats{..} =
  let reliability
        | msCount == 0 = 0.5
        | otherwise    = 1.0 - (fromIntegral msFailures / fromIntegral msCount)

      latency = speedScore (expectedResponseTime msgType) msAvgResponseTime

      (ReliabilityWeight rWeight, LatencyWeight lWeight) = messageTypeWeights msgType

      combinedScore = (reliability * rWeight) + (latency * lWeight)

  in max 0.0 $ min 1.0 $ combinedScore


-- | Calculates latency-based speed score based on response time ranges.
-- Response times below 'ideal' are perfect (1.0).
-- Between 'ideal' and 'reasonable' decay linearly to ~0.6–0.7.
-- Between 'reasonable' and 'fatal' decay further to 0.0.
-- Above 'fatal', score is 0.0 (unacceptable latency).
speedScore :: ExpectedResponseTime -> Double -> Double
speedScore (ExpectedResponseTime ideal reasonable fatal) responseTime
  | responseTime <= ideal = 1.0
  | responseTime <= reasonable =
      1.0 - ((responseTime - ideal) / (reasonable - ideal)) * 0.5
  | responseTime <= fatal =
      0.5 - ((responseTime - reasonable) / (fatal - reasonable)) * 0.5
  | otherwise = 0.0

-- | Classify a message into a performance category
classifyMessage :: Message -> MessageType
classifyMessage Hello{} = P2PWireProtocol
classifyMessage Ping = P2PWireProtocol
classifyMessage Pong = P2PWireProtocol
classifyMessage Disconnect{} = P2PWireProtocol
classifyMessage Status{} = Responses
classifyMessage GetBlockHeaders{} = HeaderRequest
classifyMessage BlockHeaders{} = Responses
classifyMessage GetBlockBodies{} = BlockRequest
classifyMessage BlockBodies{} = Responses
classifyMessage Transactions{} = Responses
classifyMessage (Blockstanbul _) = ConsensusMsg
classifyMessage GetChainDetails{} = PrivateChainRequest
classifyMessage ChainDetails{} = Responses
classifyMessage GetTransactions{} = TransactionRequest
classifyMessage GetMPNodes{} = PrivateChainRequest
classifyMessage MPNodes{} = Responses
classifyMessage NewBlockHashes{} = Responses
classifyMessage NewBlock{} = BlockResponse

-- | Assigns a reliability and latency weight to each message type
-- for the purpose of calculating peer performance scores.
--
-- The weights determine how important successful delivery (reliability)
-- and response speed (latency) are for each message category.
-- They are used by the scoring function to compute weighted quality
-- metrics for peers in the network.
--
-- TODO consider moving this to configuration for the convenience of the node
-- operator
messageTypeWeights :: MessageType -> (ReliabilityWeight, LatencyWeight)
messageTypeWeights P2PWireProtocol = (ReliabilityWeight 0.2, LatencyWeight 0.8)
messageTypeWeights Responses = (ReliabilityWeight 0.7, LatencyWeight 0.3)
messageTypeWeights HeaderRequest = (ReliabilityWeight 0.6, LatencyWeight 0.4)
messageTypeWeights BlockRequest = (ReliabilityWeight 0.7, LatencyWeight 0.3)
messageTypeWeights PrivateChainRequest = (ReliabilityWeight 0.8, LatencyWeight 0.2)
messageTypeWeights TransactionRequest = (ReliabilityWeight 0.5, LatencyWeight 0.5)
messageTypeWeights BlockResponse = (ReliabilityWeight 0.6, LatencyWeight 0.4)
messageTypeWeights ConsensusMsg = (ReliabilityWeight 0.9, LatencyWeight 0.1)

-- | Expected response time ranges for different message types (min, max in
-- milliseconds)
expectedResponseTime :: MessageType -> ExpectedResponseTime
expectedResponseTime P2PWireProtocol = ExpectedResponseTime 10 100 200
expectedResponseTime Responses = ExpectedResponseTime  50 500 800
expectedResponseTime HeaderRequest = ExpectedResponseTime 100 500 800
expectedResponseTime BlockRequest = ExpectedResponseTime 100 500 1000
expectedResponseTime PrivateChainRequest = ExpectedResponseTime 100 500 800
expectedResponseTime TransactionRequest = ExpectedResponseTime 100 500 100
expectedResponseTime BlockResponse = ExpectedResponseTime 200 30000 50000
expectedResponseTime ConsensusMsg = ExpectedResponseTime 100 1000 2000

-- | Update message statistics with new data point
--
--   The update is being calculated with a smoothing factor - exponentially
--   moving average for response time (more weight to recently added data
--   points).
--
--   This Matters for Peer Quality because:
--
--   - Network conditions change - a peer might get better/worse connectivity
--   - Recent performance is more predictive of future performance
--   - Gradual adaptation - not too sensitive to single outliers, but responsive to trends
--
-- TODO consider making alpha parameter configurable for the node operator
updateMessageStats ::  Double -> Bool -> UTCTime -> MessageStats -> MessageStats
updateMessageStats responseTime success timestamp oldStats =
  let newCount = msCount oldStats + 1
      newFailures
        | success = msFailures oldStats
        | otherwise = msFailures oldStats + 1
      alpha = 0.1
      newAvgResponseTime
        | msCount oldStats == 0 = responseTime
        | otherwise = (msAvgResponseTime oldStats * (1 - alpha)) + (responseTime * alpha)
  in MessageStats
      { msCount = newCount
      , msFailures = newFailures
      , msAvgResponseTime = newAvgResponseTime
      , msLastSeen = timestamp
      }

-- | Create an empty PeerQuality instance for a new peer with a neutral starting
-- score 0.5
emptyPeerQuality :: Point -> UTCTime -> PeerQuality
emptyPeerQuality peerId now = PeerQuality
  { pqPeerId = peerId
  , pqConnectedAt = now
  , pqLastActivity = now
  , pqMessageStats = Map.empty
  , pqOverallScore = 0.5
  }

-- | Create an empty MessageStats
emptyMessageStats :: UTCTime -> MessageStats
emptyMessageStats now = MessageStats
  { msCount = 0
  , msFailures = 0
  , msAvgResponseTime = 0.0
  , msLastSeen = now
  }
