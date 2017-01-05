{-|
Module      : Dockmaster.Config.Types
Description : Types and instances for Dockmaster Config
License     : ASL-2
Maintainer  : sam.chong.tay@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Dockmaster.Config.Types where

import Data.Yaml

import Shelly (FilePath, fromText)
import Prelude hiding (FilePath)
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP

-- | Dockmaster configuration
data Config = Config { dmcPaths :: [FilePath] }
  deriving (Eq,Show)

pathsField :: T.Text
pathsField = "PATHS"

configFields :: [T.Text]
configFields = [pathsField]

arrayFields :: [T.Text]
arrayFields = [pathsField]

-- | Custom instance to parse 'Text' directly into 'FilePath'
instance FromJSON FilePath where
    parseJSON v = fromText <$> parseJSON v

-- | Instance to parse dockmaster configuration file
instance FromJSON Config where
  parseJSON (Object v) = Config <$> (v .:? pathsField .!= [])
  parseJSON _          = fail "Top level configuration should be a hashmap."

-- | Custom instance to parse 'FilePath' directly into 'Text'
instance ToJSON FilePath where
  toJSON f = case FP.toText f of
               Left  e -> error $ "Unable to decode filepath to text field:\n" ++ T.unpack e
               Right t -> toJSON t

-- | Instance to encode dockmaster configuration
instance ToJSON Config where
  toJSON c = object
    [ pathsField .= dmcPaths c ]
