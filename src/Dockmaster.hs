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
    dockmaster
  -- * dockmaster.yml
  , module Dockmaster.Types
  , module Dockmaster.Parser
  , module Dockmaster.Compose
  -- * config.yml
  , module Dockmaster.Config.Types
  , module Dockmaster.Config.Parser
  ) where

import Dockmaster.Types
import Dockmaster.Parser
import Dockmaster.Compose
import Dockmaster.Config.Types
import Dockmaster.Config.Parser

import Shelly
import Prelude hiding (FilePath)
import Control.Monad (forM_)
import qualified Data.Text as T
default (T.Text)

-- | Runs @docker-compose@ commands against resolved composition locations
-- See usage docs for more info. Tries to find a @dockmaster.yml@ file based on
-- the initial path argument
dockmaster :: FilePath -> Bool -> T.Text -> [T.Text] -> Sh ()
dockmaster path local command args = do
  eWd <- getWorkDir path
  either dmcError dmExec eWd where
    dmcError WorkDirNotFound     = errorExit "Could not resolve dockmaster working directory."
    dmcError (DecodingError err) = echo_err "Failed to parse dm configuration." >> errorExit err
    dmExec wd = sub $ do
      cd wd
      dmYml <- dockmasterYml
      case dmYml of
        Left err    -> echo_err "Failed to parse dockmaster.yml." >> errorExit err
        Right dmYml -> do
          prepareEnv dmYml
          let hookwrap = hookWrap' dmYml command $ dockercompose dmYml $ command : args
              targets = map targetName $ dmTargets dmYml -- just grabbing machine name
          if local then hookwrap else
            forM_ targets $ \m -> dockermachine m hookwrap
