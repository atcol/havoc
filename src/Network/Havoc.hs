{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.Havoc
  ( Decision(..)
  , Listener
  , Proxy(..)
  , Strategy(..)
  , mkProxies
  , mkProxy
  ) where

import qualified Data.Binary.Builder        as B
import           System.Log.Logger

import           Control.Concurrent         (threadDelay)
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
import qualified Network.HTTP.ReverseProxy  as RP
import qualified Network.HTTP.Types.Status  as S
import qualified Network.Wai                as W
import           Network.Wai.Handler.Warp   (run)
import           System.Random              (newStdGen, randomIO, randomRs)

-- | Construct the proxies from the given list
mkProxies :: MonadIO m => [(Proxy, Maybe (Listener m))] -> [m Session]
mkProxies = map mkProxy

-- | Create a proxy
mkProxy :: MonadIO m => (Proxy, Maybe (Listener m)) -> m Session
mkProxy (x, _) = do
  mgr <- liftIO $ C.newManager C.defaultManagerSettings
  sId' <- liftIO (randomIO :: IO Int)
  session <- liftIO $ newTMVarIO (Session (show sId') 0 Nothing)
  liftIO $ run mgPort $ RP.waiProxyTo (handler session x) RP.defaultOnExc mgr
  liftIO $ atomically (takeTMVar session)
  where
    mgPort = fromMaybe 8080 (prPort x)

-- | Rewrite the request according to the proxy settings
handler :: TMVar Session -> Proxy -> (W.Request -> IO RP.WaiProxyResponse)
handler s p@(Proxy pr u _ _) r = do
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
    Reject -> do
      liftIO $ infoM pr ("Rejected request " ++ BS.unpack (W.rawPathInfo r) ++ " for proxy " ++ show p)
      return $ RP.WPRResponse (W.responseBuilder S.status500 [] B.empty)
  where
    prReq = C.parseRequest_ u
    host = C.host prReq
    reqPort = C.port prReq
    hostHdr = BS.concat [host, ":", BS.pack $ show reqPort]
    newHdrs = ("Host", hostHdr) : dropWhile ((==) ("host" :: CI BS.ByteString) . fst) (W.requestHeaders r)
    isSecure = C.secure prReq
    r' = r {W.requestHeaderHost = Just hostHdr, W.requestHeaders = newHdrs}
    redirect False = RP.WPRModifiedRequest r' (RP.ProxyDest host reqPort)
    redirect True = RP.WPRModifiedRequestSecure r' (RP.ProxyDest host reqPort)

-- | The decision mapper
decide :: Proxy -> [Float] -> W.Request -> StateT Session STM Decision
decide _ [] _ = error "Insufficient random floats" -- This could never happen?
decide (Proxy _ _ str _) (x:_) r = do
  modify (\(Session i c pr) -> Session i (c + 1) pr)
  state <- get
  let dec = strat x state str
  modify (\(Session i c _) -> Session i c (Just (r, dec)))
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
