{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Concurrent.Async
import           Data.Aeson               (eitherDecode)
import qualified Data.ByteString          as BS
import           Network.Havoc
import           Text.RawString.QQ

config = [r|
[
  { "iden": "Google-Drop", "url": "https://google.com/", "port": 1111, "strategy": { "ratio":  0.5 } },
  { "iden": "Google-Limited", "url": "https://google.com/", "port": 2222, "strategy": { "limit":  1 } },
  { "iden": "Google-Limited-Port", "url": "https://google.com/", "port": 3333, "strategy": { "limit":  1 } }
]
|]

main :: IO ()
main = do
  print config
  let c = eitherDecode config :: Either String [Proxy]
  case c of
    Left e   -> print $ "No proxy settings parsed: " ++ e
    Right c' -> do
      summarise c'
      mapConcurrently_ Prelude.id $ mkProxies c'

summarise c = print c
