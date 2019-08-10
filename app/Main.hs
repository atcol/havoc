{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Concurrent.Async
import           Data.Aeson               (eitherDecodeFileStrict)
import qualified Data.ByteString          as BS
import           Network.Havoc
import           System.Environment       (getArgs)

main :: IO ()
main = do
  c <- eitherDecodeFileStrict "settings.json" :: IO (Either String [Proxy])
  case c of
    Left e   -> print $ "No proxy settings parsed: " ++ e
    Right c' -> do
      summarise c'
      mapConcurrently_ Prelude.id $ mkProxies c'

summarise c = print c
