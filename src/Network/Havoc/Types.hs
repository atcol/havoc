{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.Havoc.Types where

import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.:))
import           Data.Foldable             (asum)
import           GHC.Generics
import qualified Network.HTTP.ReverseProxy as RP
import qualified Network.Wai               as W

-- | A sink for proxy request & response cycles
type Listener m = (Session, W.Request, RP.WaiProxyResponse) -> m ()

-- | A proxy's state across requests
data Session =
  Session
    -- | The session's request count
    { sReqCount :: Int
    -- | The previous request & decision
    , sPrev     :: Maybe (W.Request, Decision)
    }
  deriving (Show)

-- | The settings for a proxy
data Proxy =
  Proxy
  -- | The identifier for this proxy
    { iden     :: String
  -- | The URL we're reverse proxying
    , url      :: String
  -- | The behaviour
    , strategy :: Strategy
  -- | Optional port to listen on
    , port     :: Maybe Int
    }
  deriving (Show, Eq, Generic)

-- | The behaviour for a proxy
data Strategy
  -- | Let all requests pass through
  = Transparent
  -- | The number of requests to accept before failing all of them
  | ReqLimit Int
  -- | The percentage of requests to drop/fail
  | DropRatio Float
  -- | Pause request processing for the specified milliseconds
  | Delay Int
  deriving (Eq, Generic, Read, Show)

-- | The result of running a strategy
data Decision
  = Pass
  | Reject
  deriving (Eq, Show)

instance FromJSON Proxy

instance FromJSON Strategy where
  parseJSON =
    withObject "Strategy" $ \v ->
      asum [DropRatio <$> v .: "drop", ReqLimit <$> v .: "limit", Delay <$> v .: "delay", return Transparent]
