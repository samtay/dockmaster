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
import Shelly
import Prelude hiding (FilePath)
import qualified Data.Text as T
default (T.Text)

-- | Runs docker-compose but uses compiled docker-compose.yml file
-- if template/vars are specified in dockmaster.yml
dockercompose :: Dockmaster -> [T.Text] -> Sh ()
dockercompose cfg optargs = print_stdout True $ maybe
  (run_ "docker-compose" optargs) -- default run on docker-compose.yml, as usual
  (composeViaTemplate optargs)
  (dmFile cfg)

-- | Uses 'ComposeFile' argument to build templated docker-compose.yml content
-- and pipes it directly to docker-compose
--
-- TODO make sure the pipe doesnt output garbage
-- If we need shell-level pipe this is possible:
-- https://hackage.haskell.org/package/shelly-1.6.8.1/docs/Shelly.html#v:-45--124--45-
composeViaTemplate :: [T.Text] -> ComposeFile -> Sh ()
composeViaTemplate optargs cf =
  compileTemplate cf -|- run_ "docker-compose" ("-f" : "-" : optargs)

-- | Leverages @cop@ to build docker-compose.yml content from template/vars
--
-- This function returns the stdout wrapped in 'Sh'. It can then be piped to
-- docker-compose via @docker-compose -f -@ -- see documentation for docker-compose.
compileTemplate :: ComposeFile -> Sh T.Text
compileTemplate cf = do
  file <- parse (cfPath cf)
  vars <- mapM parse (cfVars cf)
  run "cop" $ ["--render-template", file] ++ vars
    where parse = toText <=< parsePath <=< toText
