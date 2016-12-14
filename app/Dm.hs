{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

-- Local packages
import Dockmaster

-- External packages
import Shelly
import Options.Applicative

-- Base packages
import System.Environment
import Prelude hiding (FilePath)
import Data.Monoid ((<>))
import qualified Data.Text as T

default (T.Text)

-- | Datatype to hold cli options/arguments
data Dm = Dm
  { dmCompositionDir :: String
  , dmVerbose        :: Bool
  , dmDcCommand      :: String
  , dmDcOpts         :: [String]
  } deriving (Eq,Show)

-- | Main runtime
-- Retrieves possible DOCKMASTER_COMPOSITION env variable for use as
-- default composition, then defers to cli parser & shelly runtime
main :: IO ()
main = do
  envCompDir <- shelly $ get_env "DOCKMASTER_COMPOSITION"
  let defaultCompDir = maybe "." T.unpack envCompDir
   in execParser (opts defaultCompDir) >>= runtime

-- | Runtime execution
--
-- Accepts 'Dm' instance and forwards to 'dm' function
runtime :: Dm -> IO ()
runtime opts = shelly $ (subVerbosity $ dmVerbose opts) $ do
  let (path, command, optargs)
        = ( T.pack $ dmCompositionDir opts
          , T.pack $ dmDcCommand opts
          , map T.pack $ dmDcOpts opts
          )
   in dm (fromText path) command optargs

-- | Accepts a verbosity setting for the subshell
-- Propogates verbosity to printing options for commands, stdout, stderr
subVerbosity :: Bool -> Sh a -> Sh a
subVerbosity v =
  (print_stdout v) . (print_stderr v) . (print_commands v)

-- | Parser for 'Dm' opts/args.
-- Takes a string argument as the default composition value
parser :: String -> Parser Dm
parser defaultCompDir = Dm
  <$> strOption
      ( long "composition"
      <> short 'c'
      <> metavar "PATH"
      <> showDefault
      <> value defaultCompDir
      <> help "Composition directory. Note this can be relative to DM_COMPOSITIONS_DIR array." )
  <*> switch
      ( long "verbose"
      <> short 'v'
      <> help "Verbose output flag" )
  <*> argument str
      ( metavar "COMMAND"
      <> help "Command to forward to docker-compose")
  <*> many (argument str
      ( metavar "ARGS"
      <> help "Any arguments/options to forward to docker-compose COMMAND"))

-- | Generate 'ParserInfo' 'Dm'.
-- Takes a string argument as the default composition value
opts :: String -> ParserInfo Dm
opts defaultCompDir = info (helper <*> parser defaultCompDir)
  (  fullDesc
  <> progDesc "Orchestrate your docker-compose"
  <> header "dm - yaml loving docker compose orchestration"
  )
