{-|
Module      : Dockmaster
Description : Runtime execution
License     : ASL-2
Maintainer  : sam.chong.tay@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster
  (
  -- * Runtime execution
    dm
  -- * dockmaster.yml
  , module Dockmaster.Types
  , module Dockmaster.Parser
  -- * config.yml
  , module Dockmaster.Config.Types
  , module Dockmaster.Config.Parser
  ) where

-- Base modules
import Data.Either
import Data.Monoid ((<>))
import Data.Maybe

-- Local modules
import Dockmaster.Types
import Dockmaster.Config.Types
import Dockmaster.Parser
import Dockmaster.Config.Parser
import Dockmaster.Compose

-- External modules
import Shelly
import Text.Regex.Posix
import Prelude hiding (FilePath)
import qualified Data.Text as T
default (T.Text)

-- | Runs @docker-compose@ commands against resolved composition locations
-- See usage docs for more info. Tries to find a @dockmaster.yml@ file based on
-- the initial path argument
dm :: FilePath -> T.Text -> [T.Text] -> Sh ()
dm path command args = do
  eWd <- getWorkDir path
  case eWd of
    Left err -> errorExit err
    Right wd -> sub $ do
      cd wd
      dmYml <- dockmasterYml
      case dmYml of
        Left err    -> echo_err "Failed to parse dockmaster.yml:\n" >> errorExit err
        Right dmYml -> do
          let targets = map targetName $ dmTargets dmYml -- just grabbing machine name
          (flip mapM_) targets $ \m -> dockermachine m $ do
            hookWrap' dmYml command $ dockercompose dmYml $ command : args

-- | Takes machine name and 'Sh' action, and wraps 'Sh' action in scope of
-- @docker-machine@ env
dockermachine :: T.Text -> Sh a -> Sh a
dockermachine m action = sub $ do
  envvars <- run "docker-machine" ["env", m]
  mapM (\(var,val) -> setenv (T.pack var) (T.pack val))
    $ pairEnvvars $ T.unpack envvars
  action

-- | Accept a string of possibly many export @VAR=VAL@ statements
-- and return the @(VAR,VAL)@ pairs
pairEnvvars :: String -> [(String,String)]
pairEnvvars ls = mapMaybe match $ lines ls
  where match :: String -> Maybe (String,String)
        match l = case l =~ exportPattern :: [[String]] of
                    [[_,var,val]] -> Just (var, val)
                    _             -> Nothing

-- | Regex pattern for matching @docker-machine@ env output
exportPattern :: String
exportPattern = "export ([A-Za-z0-9_]+)=\"(.*)\""

