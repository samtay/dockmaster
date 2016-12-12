{-# LANGUAGE OverloadedStrings #-}
module Dockmaster.Config.Types where

import Data.Yaml
import Control.Applicative
import Shelly
import Prelude hiding (FilePath)

-- | Dockmaster configuration
data Config = Config { dmcPaths :: [FilePath] }

-- | Custom instance to parse strings directly into FilePath
-- TODO ensure this isn't bad. FFP warned me against "orphan instances"...
instance FromJSON FilePath where
    parseJSON v = fromText <$> parseJSON v

-- | Instance to parse dockmaster configuration file
instance FromJSON Config where
  parseJSON (Object v) = Config
                       <$> (v .:? "PATHS" .!= [])

