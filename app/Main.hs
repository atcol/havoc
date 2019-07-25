{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified Data.ByteString   as BS
import           Data.Text         (Text)
import           Data.Yaml         (FromJSON (..), (.:))
import qualified Data.Yaml         as Y
import           Network.Havoc     (mkProxies)
import           Text.RawString.QQ

main :: IO ()
main = head $ mkProxies [("https://google.com/", 80, 8080)]