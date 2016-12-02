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

-- | Execute action in workdir (does not affect cwd outside of action)
workDirExec :: FilePath -> Sh a -> Sh (Either T.Text a)
workDirExec = undefined

-- | Resolve the appropriate docker-compose workdir (based on path arg)
-- TODO This needs error handling when dockmaster.yml not found
getWorkDir :: FilePath -> Sh (Either T.Text FilePath)
getWorkDir p
  -- search for dockmaster.yml
  -- aggregate searched dirs for verbosity msg ?
  | p /= "."  = do return $ Left "dockmaster.yml file not found"
  | otherwise = do
    wd <- pwd
    return $ Right wd

-- | Cd into workdir
-- TODO check if cd retains context when run externally (i.e. do I need to pass action here)
cdWorkDir :: FilePath -> Sh (Either T.Text (Sh ()))
cdWorkDir = undefined
