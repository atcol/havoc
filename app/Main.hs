{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger

import           Control.Concurrent.Async
import           Data.Aeson                (eitherDecodeFileStrict)
import           Network.Havoc
import           Options.Generic
import System.IO (stdout)

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

instance ParseRecord Profile

main :: IO ()
main = do
  stdOutLogger <- streamHandler stdout INFO >>= \l -> return $ setFormatter l (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (setLevel INFO . setHandlers [stdOutLogger])
  pr <- getRecord "Havoc" :: IO Profile
  loadProxies pr >>= mapConcurrently Prelude.id . mkProxies >>= mapM_ print

-- | Create our proxy instances from the specified profile
loadProxies :: Profile -> IO [(Proxy, Maybe (Listener IO))]
loadProxies (Solo u str p) = return [(Proxy "Shell" u str (Just p), Nothing :: Maybe (Listener IO))]
loadProxies (Farm f) = do
  c <- eitherDecodeFileStrict f :: IO (Either String [Proxy])
  case c of
    Left e   -> error $ "No proxy settings parsed: " ++ e
    Right c' -> return $ map (, Nothing :: Maybe (Listener IO)) c'
