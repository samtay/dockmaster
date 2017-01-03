{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Dockmaster
import Shelly
import Options.Applicative

import Prelude hiding (FilePath)
import Data.Monoid ((<>))
import Options.Utils (text, filePathOption)
import qualified Data.Text as T

default (T.Text)

-- | Datatype to hold cli options/arguments
data Dm = Dm
  { dmCompositionDir :: FilePath
  , dmVerbose        :: Bool
  , dmLocal          :: Bool
  , dmDcCommand      :: T.Text
  , dmDcOpts         :: [T.Text]
  } deriving (Eq,Show)

-- | Main runtime
-- Retrieves possible DOCKMASTER_COMPOSITION env variable for use as
-- default composition, then defers to cli parser & shelly runtime
main :: IO ()
main = do
  envCompDir <- shelly $ get_env "DOCKMASTER_COMPOSITION"
  let defaultCompDir = maybe "." T.unpack envCompDir
   in execParser (opts defaultCompDir) >>= execShelly

-- | Shelly execution
--
-- Accepts 'Dm' instance and forwards to 'dm' function
execShelly :: Dm -> IO ()
execShelly (Dm c v l command optargs) = shelly
  $ escaping False
  $ subVerbosity v
  $ dockmaster c l command optargs

-- | Accepts a verbosity setting for the subshell
-- Propogates verbosity to printing options for commands, stdout, stderr
subVerbosity :: Bool -> Sh a -> Sh a
subVerbosity v =
  print_stdout v . print_stderr v . print_commands v

-- | Parser for 'Dm' opts/args.
-- Takes a string argument as the default composition value
parser :: String -> Parser Dm
parser defaultCompDir = Dm
  <$> filePathOption
      ( long "composition"
      <> short 'c'
      <> metavar "PATH"
      <> showDefault
      <> value defaultCompDir
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

-- | Generate 'ParserInfo' 'Dm'.
-- Takes a string argument as the default composition value
opts :: String -> ParserInfo Dm
opts defaultCompDir = info (helper <*> parser defaultCompDir)
  (  fullDesc
  <> progDesc "Orchestrate your docker-compose"
  <> header "dm - yaml loving docker compose orchestration"
  )
