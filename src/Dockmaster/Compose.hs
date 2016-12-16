{-|
Module      : Dockmaster.Compose
Description : Executing docker-compose with possibly COP templated content
License     : ASL-2
Maintainer  : sam.chong.tay@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster.Compose
  ( -- * Orchestration for docker-compose.yml
    dockercompose
  ) where

-- Local modules
import Dockmaster.Types
import Dockmaster.Utils (parsePath, toText)

-- External modules
import Control.Monad ((<=<))
import Data.List (intersperse)
import Shelly
import Prelude hiding (FilePath)
import qualified Data.Text as T
default (T.Text)

-- | Runs @docker-compose@ but uses 'Dockmaster' configuration to possibly
--
-- (1) Provide multiple templated docker-compose.yml files
-- (2) Pass global @docker-compose@ flags on every call
dockercompose :: Dockmaster -> [T.Text] -> Sh ()
dockercompose cfg cliArgs = do
  let dcfg    = dmCompose cfg
      dcflags = dcFlags dcfg
      dcfiles = dcFiles dcfg
  fargs <- composeTemplates dcfiles
  print_stdout True $
    run_ "docker-compose" $ dcflags ++ cliArgs ++ intersperse "-f" fargs
  decomposeTemplates fargs

-- | Compiles compose files specified in 'Dockmaster' configuration and
-- returns a list of the composed filenames that now live in the working directory.
composeTemplates :: [ComposeFile] -> Sh [T.Text]
composeTemplates = (mapM composeTemplate) . (zip [0..])

-- | Compiles a compose file. May or may not use COP depending on 'cfTemplate' flag.
--
-- Notice this function accepts an integer along with the 'ComposeFile'; this is to
-- help identify it as a unique file.
composeTemplate :: (Integer, ComposeFile) -> Sh T.Text
composeTemplate = undefined

-- | Dual to 'composeTemplates', this accepts the list of composed filenames
-- and removes them.
decomposeTemplates :: [T.Text] -> Sh ()
decomposeTemplates = undefined

-- | Leverages @cop@ to build @docker-compose.yml@ content from template/vars.
--
-- This function returns the stdout wrapped in 'Sh'. It can then be piped to
-- @docker-compose@ via @docker-compose -f -@ -- see documentation for @docker-compose@.
{--
compileTemplate :: ComposeFile -> Sh T.Text
compileTemplate cf = do
  file <- parse (cfPath cf)
  vars <- mapM parse (cfVars cf)
  run "cop" $ ["--render-template", file] ++ vars
    where parse = toText <=< parsePath <=< toText
--}
