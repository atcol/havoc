{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent.Async
import           Data.Aeson               (eitherDecodeFileStrict)
import           Network.Havoc
import           Options.Generic

-- | The program's settings/configuration
data Profile
  = Solo
      { url     :: String
      , strat   :: Strategy
      , port    :: Int
      }
  | Farm
      { file    :: FilePath
      }
  deriving (Generic, Show)

instance ParseRecord Strategy

instance ParseField Strategy

instance ParseFields Strategy

instance ParseRecord Profile

main :: IO ()
main = do
  pr <- getRecord "Havoc" :: IO Profile
  proxies <- loadProxies pr
  mapConcurrently_ Prelude.id $ mkProxies proxies

-- | Create our proxy instances from the specified profile
loadProxies :: Profile -> IO [(Proxy, Maybe (Listener IO))]
loadProxies (Solo u str p) = return [(Proxy "Shell" u str (Just p), Nothing :: Maybe (Listener IO))]
loadProxies (Farm f) = do
  c <- eitherDecodeFileStrict f :: IO (Either String [Proxy])
  case c of
    Left e -> error $ "No proxy settings parsed: " ++ e
    Right c' -> return $ map (, Nothing :: Maybe (Listener IO)) c'
