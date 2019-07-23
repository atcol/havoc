{-# LANGUAGE OverloadedStrings #-}
module Network.Havoc
  (mkProxies)
where

import qualified Data.ByteString           as BS
import           Data.Conduit.Network
import qualified Network.HTTP.Client       as C
import           Network.HTTP.ReverseProxy
import           Network.Wai               (Request)
import           Network.Wai.Handler.Warp  (run)

-- | The wrapper for matching some payload
--TODO: data (Show m) => Matcher = Matcher m (m -> Bool) deriving (Show)

type Proxy = IO ()

type URL = BS.ByteString

mkProxies :: [(URL, Int, Int)] -> [Proxy]
mkProxies []           = []
mkProxies ((u,a,b):xs) = mkProxy b u a : mkProxies xs

mkProxy lp u p = do
  mgr <- C.newManager C.defaultManagerSettings
  run lp (waiProxyTo (handler u p) defaultOnExc mgr)

handler :: URL -> Int -> (Request -> IO WaiProxyResponse)
handler u p = \r -> return $ WPRProxyDest (ProxyDest u p)
