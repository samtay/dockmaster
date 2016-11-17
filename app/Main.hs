{-# LANGUAGE OverloadedStrings #-}
module Main where

import Dockmaster.Types

import Data.Yaml
import qualified Data.ByteString as BS

main = do
  ymlData <- BS.readFile "dockmaster.yml"
  let d = decodeEither ymlData :: Either String Dockmaster
  print d
