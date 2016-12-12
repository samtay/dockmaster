{-# LANGUAGE OverloadedStrings #-}
module Dockmaster.Config.Types where

import Data.Yaml
import Control.Applicative
import Data.Text as T

-- | Dockmaster configuration
data Config = Config { dmcPaths :: [T.Text] }

-- | Instance to parse dockmaster configuration file
instance FromJSON Config where
  parseJSON (Object v) = Config
                       <$> (v .:? "PATHS" .!= [])

