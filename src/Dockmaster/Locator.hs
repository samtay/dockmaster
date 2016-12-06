{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster.Locator
  ( workDirExec
  , getWorkDir
  , cdWorkDir
  ) where

import Shelly
import Prelude hiding (FilePath)
import qualified Data.Text as T
default (T.Text)

-- | Resolve the appropriate docker-compose workdir (based on path arg)
-- TODO This needs to search paths in ~/.dockmaster/config !!
  -- aggregate searched dirs for verbosity msg ?
getWorkDir :: FilePath -> Sh (Either T.Text FilePath)
getWorkDir p = do
  found <- test_e (p </> "dockmaster.yml")
  return $ if found
     then Right p
     else Left "dockmaster.yml file not found"

-- | Execute action in workdir (does not affect cwd outside of action)
workDirExec :: FilePath -> Sh a -> Sh (Either T.Text a)
workDirExec = undefined

-- | Cd into workdir
-- TODO check if cd retains context when run externally (i.e. do I need to pass action here)
cdWorkDir :: FilePath -> Sh (Either T.Text (Sh ()))
cdWorkDir = undefined
