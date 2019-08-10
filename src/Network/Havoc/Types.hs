{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Havoc.Types where

import           Data.Aeson      (FromJSON, parseJSON, withObject, (.:))
import qualified Data.ByteString as BS
import           Data.Foldable   (asum)
import           GHC.Generics

-- | The settings for a proxy
data Proxy = Proxy {
  -- | The identifier for this proxy
  iden       :: String
  -- | The URL we're reverse proxying
  , url      :: String
  -- | The behaviour
  , strategy ::  Strategy
  -- | Optional port to listen on
  , port     :: Maybe Int
  } deriving (Show, Eq, Generic)

-- | The behaviour for a proxy
data Strategy = Transparent | ReqLimit Int | DropRatio Float
  deriving (Eq, Generic, Read, Show)

-- | The result of running a strategy
data Decision = Pass | Reject deriving (Eq, Show)

instance FromJSON Proxy where

instance FromJSON Strategy where
  parseJSON = withObject "Strategy" $ \v -> asum [
      DropRatio <$> v .: "ratio"
    , ReqLimit <$> v .: "limit"
    , return Transparent 
    ]
