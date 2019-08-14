{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.Havoc
  ( Proxy(..)
  , Decision(..)
  , Strategy(..)
  , mkProxies
  , mkProxy
  ) where

import           Control.Concurrent.STM     (STM, TMVar, atomically, newTMVarIO,
                                             putTMVar, takeTMVar)
import           Control.Monad.IO.Class     (MonadIO (..), liftIO)
import           Control.Monad.State.Strict (StateT (..), get, modify,
                                             runStateT)
import qualified Data.ByteString.Char8      as BS
import           Data.CaseInsensitive       (CI (..))
import           Data.Maybe                 (fromMaybe)
import           Network.Havoc.Types
import qualified Network.HTTP.Client        as C
import           Network.HTTP.ReverseProxy
import qualified Network.Wai                as W
import           Network.Wai.Handler.Warp   (run)
import           System.Random              (newStdGen, randomRs)
import Control.Concurrent (threadDelay)

-- | A proxy's state across requests
data Session =
  Session
    -- | The session's request count
    { sReqCount :: Int
    -- | The previous request & decision
    , sPrev     :: Maybe (W.Request, Decision)
    }
  deriving (Show)

-- | Construct the proxies from the given list
mkProxies :: [Proxy] -> [IO ()]
mkProxies = map mkProxy

-- | Create a proxy
mkProxy :: MonadIO m => Proxy -> m ()
mkProxy x = do
  mgr <- liftIO $ C.newManager C.defaultManagerSettings
  session <- liftIO $ newTMVarIO (Session 0 Nothing)
  liftIO $ run mgPort $ waiProxyTo (handler session x) defaultOnExc mgr
  where
    mgPort = fromMaybe 8080 (port x)

-- | Rewrite the request according to the proxy settings
handler :: TMVar Session -> Proxy -> (W.Request -> IO WaiProxyResponse)
handler s p@(Proxy _ u _ _) r = do
  g <- newStdGen
  _ <- prep p
  resp <-
    atomically
      (do ses <- takeTMVar s
          (resp, newSes) <- runStateT (decide p (randomRs (0.0, 1.0) g) r) ses
          putTMVar s newSes
          return resp)
  case resp of
    Pass -> return $ redirect isSecure
    Reject -> error "Rejected"
  where
    prReq = C.parseRequest_ u
    host = C.host prReq
    reqPort = C.port prReq
    newHdrs = ("Host", hostHdr) : dropWhile ((==) ("host" :: CI BS.ByteString) . fst) (W.requestHeaders r)
    hostHdr = BS.concat [host, ":", BS.pack $ show reqPort]
    isSecure = C.secure prReq
    r' = r {W.requestHeaderHost = Just hostHdr, W.requestHeaders = newHdrs}
    redirect False = WPRModifiedRequest r' (ProxyDest host reqPort)
    redirect True = WPRModifiedRequestSecure r' (ProxyDest host reqPort)

-- | The decision mapper
decide :: Proxy -> [Float] -> W.Request -> StateT Session STM Decision
decide _ [] _ = error "Insufficient random floats" -- This could never happen?
decide (Proxy _ _ str _) (x:_) r = do
  modify (\(Session c pr) -> Session (c + 1) pr)
  state <- get
  let dec = strat x state str
  modify (\(Session c _) -> Session c (Just (r, dec)))
  return dec

-- | Apply the strategy according to the session
strat :: Float -> Session -> Strategy -> Decision
strat _ _ Transparent = Pass
strat _ s (ReqLimit n) =
  if n < sReqCount s
    then Reject
    else Pass
strat x _ (DropRatio n) =
  if x > n
    then Reject
    else Pass
strat _ _ (Delay _) = Pass

-- | Prepare the response or act prior to deciding
prep :: MonadIO m => Proxy -> m (Maybe a)
prep (Proxy _ _ (Delay ms) _) = liftIO (threadDelay (ms * 1000)) >> return Nothing
prep _ = return Nothing

