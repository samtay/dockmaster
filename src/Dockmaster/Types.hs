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

import Data.Yaml
import Dockmaster.Config.Types

import Data.HashMap.Lazy (HashMap, member)
import Data.Monoid ((<>))
import Shelly (FilePath)
import Prelude hiding (FilePath)
import qualified Data.Text as T

-- | Dockmaster configuration (specified by dockmaster.yml)
data Dockmaster = Dockmaster
  { dmCompose  :: ComposeConfig
  , dmEnv      :: EnvConfig
  , dmTargets  :: [Target]
  , dmCommands :: HashMap T.Text CommandConfig
  } deriving (Show,Eq)

-- | Configuration for @docker-compose.yml@ arguments.
data ComposeConfig = ComposeConfig
  { dcFiles     :: [ComposeFile]
  , dcFlags     :: [T.Text]
  } deriving (Show,Eq)

-- | Configuration for docker-compose compose file construction.
data ComposeFile = ComposeFile
  { cfPath     :: FilePath
  , cfTemplate :: Bool
  , cfConfig   :: [FilePath]
  } deriving (Show,Eq)

-- | Global environment variables for dockmaster execution.
data EnvConfig = EnvConfig
  { ecFiles :: [FilePath] 
  , ecVars  :: HashMap T.Text T.Text
  } deriving (Show,Eq)

-- | Targets are used to identify where compositions are run.
data Target = Target
  { targetName    :: T.Text
  , targetType    :: T.Text
  , targetMachine :: Maybe T.Text
  } deriving (Show,Eq)

-- | Hooks can be specified by filename or direct shell command.
data Hook = File T.Text | Shell T.Text deriving (Show,Eq)

-- | Configuration for each command.
data CommandConfig = CommandConfig
  { ccRunCompose :: Bool
  , ccPreHooks   :: [Hook]
  , ccPostHooks  :: [Hook]
  } deriving (Show,Eq)

-- | 'FromJSON' instance for 'ComposeFile'
instance FromJSON ComposeFile where
  parseJSON (Object v) = ComposeFile
                         <$> v .:  "path"
                         <*> (isTemplate <$> (v .:? "type"))
                         <*> v .:? "config"            .!= []

-- | Small helper method to parse dockmaster/compose/files/type.
isTemplate :: Maybe String -> Bool
isTemplate (Just "template") = True
isTemplate _                 = False

-- | 'FromJSON' instance for 'ComposeConfig'
instance FromJSON ComposeConfig where
  parseJSON (Object v) = ComposeConfig
                         <$> v .:? "files" .!= []
                         <*> (concatMap T.words <$> v .:? "flags" .!= [])

-- | 'Monoid' instance for 'ComposeConfig'
instance Monoid ComposeConfig where
  mempty = ComposeConfig [] []
  mappend (ComposeConfig ws xs) (ComposeConfig ys zs) =
    ComposeConfig (ws ++ ys) (xs ++ zs)

-- | 'FromJSON' instance for 'EnvConfig'
instance FromJSON EnvConfig where
   parseJSON (Object v) = EnvConfig
                         <$> v .:? "files" .!= []
                         <*> v .:? "vars" .!= mempty

-- | 'Monoid' instance for 'EnvConfig'
instance Monoid EnvConfig where
  mempty = EnvConfig [] mempty
  mappend (EnvConfig ws xs) (EnvConfig ys zs) =
    EnvConfig (ws <> ys) (xs <> zs)

-- | 'FromJSON' instance for 'Target'
instance FromJSON Target where
  parseJSON (Object v) = Target
                         <$> v .:  "name"
                         <*> v .:  "type"
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
                         <$> v .:?  "run_compose" .!= True
                         <*> v .:?  "pre_hooks"   .!= []
                         <*> v .:?  "post_hooks"  .!= []

-- | 'FromJSON' instance for 'Dockmaster'
instance FromJSON Dockmaster where
  parseJSON (Object v) = Dockmaster
                         <$> v .:?  "compose"  .!= mempty
                         <*> v .:?  "env"      .!= mempty
                         <*> v .:?  "targets"  .!= []
                         <*> v .:?  "commands" .!= mempty
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse Dockmaster from YAML/JSON"
