{-# LANGUAGE DeriveGeneric #-}
module Blockchain.PeerQuality (
  -- * Data Types
  PeerQuality (..),
  MessageType (..),

  -- * Message Classification
  classifyMessage,

  -- * Utility Functions
  emptyPeerQuality,
) where

import Crypto.Types.PubKey.ECC (Point)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Blockchain.Data.Wire (Message (..))

-- | Complete quality assessment for a peer.
data PeerQuality = PeerQuality
  { pqPeerId       :: !Point
  -- ^ Peer's public key
  , pqConnectedAt  :: !UTCTime
  -- ^ When connection was established
  , pqLastActivity :: !UTCTime
  -- ^ Last message activity
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

-- | Create an empty PeerQuality instance for a new peer with a neutral starting
-- score 0.5
emptyPeerQuality :: Point -> UTCTime -> PeerQuality
emptyPeerQuality peerId now = PeerQuality
  { pqPeerId = peerId
  , pqConnectedAt = now
  , pqLastActivity = now
  , pqOverallScore = 0.5
  }
