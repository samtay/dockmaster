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
import Dockmaster.Utils (parsePath', toText)

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
    run_ "docker-compose" $ dcflags ++ intersperse' "-f" fargs ++ cliArgs
  decomposeTemplates

-- | Compiles compose files specified in 'Dockmaster' configuration and
-- returns a list of the composed filenames that now live in the working directory.
composeTemplates :: [ComposeFile] -> Sh [T.Text]
composeTemplates = (mapM composeTemplate) . (zip [0..])

-- | Compiles a compose file. May or may not use COP depending on 'cfTemplate' flag.
-- Returns filename.
--
-- Notice this function accepts an integer along with the 'ComposeFile'; this is to
-- help identify it as a unique file.
composeTemplate :: (Integer, ComposeFile) -> Sh T.Text
composeTemplate (_, ComposeFile {cfPath=f, cfTemplate=False}) = parsePath' f
composeTemplate (n, ComposeFile {cfPath=f, cfConfig=cs}) = do
  let tmpName = T.concat [tmpFilePrefix, (T.pack $ show n), ".yml"]
      tmpFile = fromText tmpName
  compiledYml <- compileTemplate f cs
  rm_f tmpFile -- might as well remove in case left over from failure
  writefile tmpFile compiledYml
  return tmpName

-- | Removes all ".dm_tmp_compose_*" files in current working directory.
decomposeTemplates :: Sh ()
decomposeTemplates = do
  files <- lsT $ fromText "."
  mapM_ fn files
    where fn file = when (tmpFilePrefix `T.isPrefixOf` file)
            $ rm (fromText file)

-- | Leverages @cop@ to build @docker-compose.yml@ content from template/vars.
--
-- This function returns the stdout wrapped in 'Sh'.
compileTemplate :: FilePath -> [FilePath] -> Sh T.Text
compileTemplate f cs = do
  template <- parsePath' f
  vars <- mapM parsePath' cs
  cop template vars

-- | COP render template wrapper
cop :: T.Text -> [T.Text] -> Sh T.Text
cop template vars = run "cop" $ "--render-template" : template : vars

-- | Prefix for temporary compose files
tmpFilePrefix :: T.Text
tmpFilePrefix = ".dm_tmp_compose_"

-- | Slight modification to 'Data.List.intersperse', also prefixes when nonempty.
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' a xs = a : intersperse a xs
