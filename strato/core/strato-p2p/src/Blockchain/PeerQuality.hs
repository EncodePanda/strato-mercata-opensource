{-# LANGUAGE DeriveGeneric #-}
module Blockchain.PeerQuality (
   -- * Data Types
   PeerQuality (..)
 , MessageType (..)
 , MessageStats (..)

   -- * Message Classification
 , classifyMessage

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
