{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Havoc.Types where

import           Data.Aeson
import qualified Data.ByteString as BS
import           Data.Foldable   (asum)
import           GHC.Generics

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

data Strategy = ReqLimit Int | DropRatio Float
  deriving (Show, Eq, Generic)

instance FromJSON Proxy where

instance FromJSON Strategy where
  parseJSON = withObject "Strategy" $ \v -> asum [
    DropRatio <$> v .: "ratio",
    ReqLimit <$> v .: "limit"
    ]
