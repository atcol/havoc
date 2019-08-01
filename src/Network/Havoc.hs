{-# LANGUAGE OverloadedStrings #-}
module Network.Havoc (
  Proxy (..)
  , Decision (..)
  , Strategy (..)
  , mkProxies
) where

import           Control.Concurrent.MVar      (MVar (..), newMVar, putMVar,
                                               takeMVar)
import           Control.Monad                (liftM)
import           Control.Monad.IO.Class       (MonadIO (..), liftIO)
import           Control.Monad.State.Strict   (StateT (..), get, modify,
                                               runStateT)
import           Control.Monad.STM            (STM (..))
import           Control.Monad.Trans.Resource (ResourceT (..))
import qualified Data.ByteString.Char8        as BS
import           Data.CaseInsensitive         (CI (..))
import           Data.Char                    (toLower)
import           Data.Conduit.Network
import           Data.HashMap.Strict
import           Data.Maybe                   (fromMaybe, maybe)
import           Debug.Trace                  (trace)
import           Network.Havoc.Types
import qualified Network.HTTP.Client          as C
import           Network.HTTP.ReverseProxy
import qualified Network.HTTP.Types           as H
import qualified Network.Wai                  as W
import           Network.Wai.Handler.Warp     (run)

type URL = BS.ByteString

data Session = Session {
                -- | The session's request count
                sReqCount :: Int
              } deriving (Eq, Show)

mkProxies :: [Proxy] -> [IO ()]
mkProxies []     = []
mkProxies (x:xs) = mkProxy x : mkProxies xs

-- | Create a proxy
mkProxy :: MonadIO m => Proxy -> m ()
mkProxy x = do
  mgr <- liftIO $ C.newManager C.defaultManagerSettings
  session <- liftIO $ newMVar (Session 0)
  liftIO $ run mgPort $ waiProxyTo (handler session x) defaultOnExc mgr
  where mgPort = fromMaybe 8080 (port x)

-- | Rewrite the request according to the proxy settings
handler :: MVar Session -> Proxy -> (W.Request -> IO WaiProxyResponse)
handler s (Proxy _ u _ _) r = do
  ses <- liftIO $ takeMVar s
  (resp, newSes) <- runStateT (decide r) ses
  liftIO $ putMVar s newSes
  case resp of
    Pass   -> return $ redirect isSecure
    Reject -> error "Rejected"
    where prReq = C.parseRequest_ u
          host = C.host prReq
          port = C.port prReq
          newHdrs = [("Host", hostHdr)] ++ (dropWhile ((==) ("host" :: CI BS.ByteString) . fst) $ W.requestHeaders r)
          hostHdr = BS.concat [host, ":", BS.pack $ show port]
          isSecure = C.secure prReq
          r' = r { W.requestHeaderHost = Just hostHdr
                 , W.requestHeaders = newHdrs
                 }
          redirect False = WPRModifiedRequest r' (ProxyDest host port)
          redirect True  = WPRModifiedRequestSecure r' (ProxyDest host port)

decide :: W.Request -> StateT Session (IO) Decision
decide r = do
  modify (\s -> Session ((sReqCount s) + 1))
  latest <- get
  trace ("new state is " ++ show latest)  $ return Pass
