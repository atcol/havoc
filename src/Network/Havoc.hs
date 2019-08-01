{-# LANGUAGE OverloadedStrings #-}
module Network.Havoc (
  Proxy (..)
  , Strategy (..)
  , mkProxies
) where

import qualified Data.ByteString.Char8     as BS
import           Data.CaseInsensitive      (CI (..))
import           Data.Char                 (toLower)
import           Data.Conduit.Network
import           Data.Maybe                (fromMaybe, maybe)
import           Debug.Trace               (trace)
import           Network.Havoc.Types
import qualified Network.HTTP.Client       as C
import           Network.HTTP.ReverseProxy
import qualified Network.HTTP.Types        as H
import qualified Network.Wai               as W
import           Network.Wai.Handler.Warp  (run)

type URL = BS.ByteString

mkProxies :: [Proxy] -> [IO ()]
mkProxies []     = []
mkProxies (x:xs) = mkProxy x : mkProxies xs

-- |
mkProxy :: Proxy -> IO ()
mkProxy x = do
  mgr <- C.newManager C.defaultManagerSettings
  run mgPort $ waiProxyTo (handler x) defaultOnExc mgr
  where mgPort = fromMaybe 8080 (port x)

-- | Rewrite the request according to the proxy settings
handler :: Proxy -> (W.Request -> IO WaiProxyResponse)
handler (Proxy _ u _ _) r = do
  return $ redirect isSecure
    where prReq = C.parseRequest_ u
          host = C.host prReq
          port = C.port prReq
          newHdrs = [("Host", hostHdr)] ++ (dropWhile ((==) ("host" :: CI BS.ByteString) . fst) $ W.requestHeaders r)
          hostHdr = BS.concat [host, ":", BS.pack $ show port]
          isSecure = C.secure prReq
          r' = r { W.requestHeaderHost = Just hostHdr
                 , W.requestHeaders = newHdrs
                 }
          redirect False = WPRModifiedRequest (trace ("new r' is " ++ show r') r') (ProxyDest host port)
          redirect True = WPRModifiedRequestSecure (trace ("new r' is " ++ show r') r') (ProxyDest host port)
