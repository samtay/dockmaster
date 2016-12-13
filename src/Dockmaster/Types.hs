{-|
Module      : Dockmaster.Types
Description : Dockmaster types and instances for dockmaster.yml
License     : ASL-2
Maintainer  : sam.chong.tay@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Dockmaster.Types where

import Dockmaster.Config.Types

import Data.Yaml
import Control.Applicative
import Data.HashMap.Lazy (HashMap, lookup, member)
import Data.Monoid (mempty)
import Shelly
import Prelude hiding (lookup, FilePath)
import qualified Data.Text as T

-- | Dockmaster configuration (specified by dockmaster.yml)
data Dockmaster = Dockmaster { dmFile   :: Maybe ComposeFile
                             , dmTargets :: [Target]
                             , dmCommands :: HashMap T.Text CommandConfig
                             } deriving (Show)

-- | Configuration for @docker-compose.yml@ file template & vars.
data ComposeFile = ComposeFile { cfPath :: FilePath
                               , cfVars :: [FilePath]
                               } deriving (Show)

-- | Targets are used to identify where compositions are run.
data Target = Target { targetName    :: T.Text
                     , targetType    :: T.Text
                     , targetMachine :: Maybe T.Text
                     } deriving (Show)

-- | Hooks can be specified by filename or direct shell command.
data Hook = File T.Text | Shell T.Text deriving (Show)

-- | Configuration for each command.
data CommandConfig = CommandConfig { ccCompose :: Bool
                                   , ccPreHooks :: [Hook]
                                   , ccPostHooks :: [Hook]
                                   } deriving (Show)

-- | 'FromJSON' instance for 'ComposeFile'
instance FromJSON ComposeFile where
  parseJSON (Object v) = ComposeFile
                         <$> v .: "path"
                         <*> v .:? "config" .!= []

-- | 'FromJSON' instance for 'Target'
instance FromJSON Target where
  parseJSON (Object v) = Target
                         <$> v .: "name"
                         <*> v .: "type"
                         <*> v .:? "machine"

-- | 'FromJSON' instance for 'Hook'
instance FromJSON Hook where
  parseJSON (Object v)
    | member "file" v  = File  <$> v .: "file"
    | member "shell" v = Shell <$> v .: "shell"
    | otherwise        = error "Hooks are specified by file or shell only"

-- | 'FromJSON' instance for 'CommandConfig'
instance FromJSON CommandConfig where
  parseJSON (Object v) = CommandConfig
                         <$> v .:?  "compose" .!= True
                         <*> v .:?  "pre_hooks" .!= []
                         <*> v .:?  "post_hooks" .!= []

-- | 'FromJSON' instance for 'Dockmaster'
instance FromJSON Dockmaster where
  parseJSON (Object v) = Dockmaster
                         <$> v .:?  "file"
                         <*> v .:?  "targets" .!= []
                         <*> v .:?  "commands" .!= mempty
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse Dockmaster from YAML/JSON"
