{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Havoc (mkProxies)

main :: IO ()
main = head $ mkProxies [("https://google.com/", 80, 8080)]
