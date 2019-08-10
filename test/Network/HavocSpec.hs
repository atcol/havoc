{-# LANGUAGE OverloadedStrings #-}

module Network.HavocSpec
  ( spec
  ) where

import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy      as BS
import           Network.Havoc
import           Network.Havoc.Types
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Client       (HttpException)
import           Network.HTTP.Types        (status200)
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.Status (statusCode)
import           Network.Wai               (Application, responseLBS)
import           Network.Wai.Handler.Warp  (run)
import           Test.Hspec
import           Test.Hspec.Wai

-- | The number of tests we run per proxy
testLimit :: Int
testLimit = 10

proxyTransparent = Proxy "Local" "http://localhost:8080/" Transparent (Just 1111)

proxyReqLimit = Proxy "Local" "http://localhost:8080/" (ReqLimit 5) (Just 2222)

proxyDropRatio = Proxy "Local" "http://localhost:8080/" (DropRatio 0.6) (Just 3333)

-- | The Warp application for testing proxies against
warp :: IO ()
warp = run 8080 app >> print "Done"

-- | The WAI application for Warp
app :: Application
app req f = f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

-- | The testing spec.
spec :: Spec
spec = do
  let server = async warp >> print "Warp Completed"
  with server $
    describe "Havoc" $ do
      it "supports Transparent" $
        withAsync
          (mkProxy proxyTransparent)
          (\r' -> do
             statusCodes <-
               mapM
                 (\n -> do
                    r <- req "http://localhost:1111/"
                    return $ statusCode $ responseStatus r)
                 [1 .. testLimit]
             countMatching 200 statusCodes `shouldSatisfy` (==) 10)
      it "supports ReqLimit" $
        withAsync
          (mkProxy proxyReqLimit)
          (\r' -> do
             threadDelay 1000000
             statusCodes <-
               mapM
                 (\n -> do
                    r <- req "http://localhost:2222/"
                    return $ statusCode $ responseStatus r)
                 [1 .. testLimit]
             countMatching 200 statusCodes `shouldBe` 5
             countMatching 500 statusCodes `shouldBe` 5)
      it "supports DropRatio" $
        withAsync
          (mkProxy proxyDropRatio)
          (\r' -> do
             threadDelay 1000000
             statusCodes <-
               mapM
                 (\n -> do
                    r <- req "http://localhost:3333/"
                    return $ statusCode $ responseStatus r)
                 [1 .. testLimit]
             let sucCount = countMatching 200 statusCodes
             sucCount `shouldSatisfy` (<=) 4
             countMatching 500 statusCodes `shouldBe` 10 - sucCount)

countMatching s = length . filter (s ==)

req :: String -> IO (Response BS.ByteString)
req url = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest url
  r <- try (httpLbs request manager) :: IO (Either HttpException (Response BS.ByteString))
  case r of
    Left e   -> error $ "Failed with: " ++ show e
    Right r' -> return r'
