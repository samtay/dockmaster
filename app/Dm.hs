{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Dockmaster
import Shelly
import Options.Applicative
import Options.Utils

import Prelude hiding (FilePath)
import Data.Monoid ((<>))
import Control.Monad (join)
import qualified Data.Text as T

default (T.Text)

-- | Main runtime
--
-- Retrieves possible @DOCKMASTER_COMPOSITION@ env variable for use as
-- default composition, then defers to cli parser & shelly runtime
main :: IO ()
main = do
  envCompDir <- shelly $ get_env "DOCKMASTER_COMPOSITION"
  let defaultCompDir = maybe "." T.unpack envCompDir
  join . execParser' $
    info (helper <*> versionOption <*> parser defaultCompDir)
    (  fullDesc
    <> progDesc "Orchestrate your docker-compose"
    <> header "dm - yaml loving docker compose orchestration"
    )

-- | Parse CLI opts/args into arguments for 'execDm'
parser :: String -> Parser (IO ())
parser defaultCompDir =
  execDm
    <$> filePathOption
        ( long "composition"
        <> short 'c'
        <> metavar "PATH"
        <> showDefault
        <> value defaultCompDir
        <> completer availableCompositions
        <> help "Composition directory. Note this can be relative to any directories specified in global config." )
    <*> switch
        ( long "verbose"
        <> short 'v'
        <> help "Verbose output.")
    <*> switch
        ( long "local"
        <> short 'l'
        <> help "Execute without connecting to configured docker machine." )
    <*> argument text
        ( metavar "COMMAND"
        <> help "Command to forward to docker-compose.")
    <*> many (argument text
        ( metavar "ARGS"
        <> help "Any arguments/options to forward to docker-compose COMMAND."))

-- | Shelly execution
--
-- Sets up shelly environment and forwards to 'dockmaster' function
execDm :: FilePath -- ^ Composition
       -> Bool     -- ^ Verbosity flag
       -> Bool     -- ^ Local-only flag
       -> T.Text   -- ^ Command
       -> [T.Text] -- ^ Opts/args for Command
       -> IO ()
execDm c v l command optargs = shelly
  $ escaping False
  $ subVerbosity v
  $ dockmaster c l command optargs

-- | Accepts a verbosity setting for the subshell
--
-- Propogates verbosity to printing options for commands, stdout, stderr
subVerbosity :: Bool -> Sh a -> Sh a
subVerbosity v =
  print_stdout v . print_stderr v . print_commands v

-- | Completer for composition option
availableCompositions :: Completer
availableCompositions = listIOCompleter $ shelly $
  either (const []) (map T.unpack)
    <$> getAvailableCompositions
