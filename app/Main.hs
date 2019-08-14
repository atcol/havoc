{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent.Async
import Data.Maybe (fromMaybe)
import           Data.Aeson               (eitherDecodeFileStrict)
import qualified Data.ByteString.Char8    as BS
import           Network.Havoc
import           Options.Generic
import           System.Environment       (getArgs)
import Control.Exception (try)

-- | The program's settings/configuration
data Profile
  = Solo
      { url   :: String
      , strat :: Strategy
      , port  :: Int
      }
  | Farm
      { file :: FilePath
      }
  deriving (Generic, Show)
  
instance ParseRecord Strategy
instance ParseField Strategy
instance ParseFields Strategy
instance ParseRecord Proxy
instance ParseRecord Profile

main :: IO ()
main = do
  pr <- getRecord "Havoc" :: IO Profile
  proxies <- loadProxies pr
  mapConcurrently_ Prelude.id $ mkProxies proxies

-- | Create our proxy instances from the specified profile
loadProxies :: Profile -> IO [Proxy]
loadProxies (Solo u str p) = return [Proxy "Shell" u str (Just p)]
loadProxies (Farm f) = do
  c <- eitherDecodeFileStrict f :: IO (Either String [Proxy])
  case c of
    Left e -> error $ "No proxy settings parsed: " ++ e
    Right c' -> return c'

-- | Load the given strategy from a raw String
parseStrategy :: String -> Strategy
parseStrategy = read
