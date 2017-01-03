{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Dockmaster
import Shelly
import Options.Applicative

import Prelude hiding (FilePath)
import Control.Monad (forM_)
import Data.Monoid ((<>))
import Options.Utils (text, filePathOption)
import qualified Data.Text as T

default (T.Text)

-- | Datatype to hold cli options/arguments
data Dm = Dm
  { dmCompositionDir :: FilePath
  , dmVerbose        :: Bool
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
execShelly opts = shelly
  $ escaping False
  $ subVerbosity (dmVerbose opts)
  $ let (Dm path _ command optargs) = opts
        in dockmaster path command optargs

-- | Runs @docker-compose@ commands against resolved composition locations
-- See usage docs for more info. Tries to find a @dockmaster.yml@ file based on
-- the initial path argument
dockmaster :: FilePath -> T.Text -> [T.Text] -> Sh ()
dockmaster path command args = do
  eWd <- getWorkDir path
  case eWd of
    Left err -> errorExit err
    Right wd -> sub $ do
      cd wd
      dmYml <- dockmasterYml
      case dmYml of
        Left err    -> echo_err "Failed to parse dockmaster.yml:\n" >> errorExit err
        Right dmYml -> do
          prepareEnv dmYml
          let targets = map targetName $ dmTargets dmYml -- just grabbing machine name
          forM_ targets $ \m -> dockermachine m $ do
            hookWrap' dmYml command $ dockercompose dmYml $ command : args

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
      <> help "Composition directory. Note this can be relative to DM_COMPOSITIONS_DIR array." )
  <*> switch
      ( long "verbose"
      <> short 'v'
      <> help "Verbose output flag" )
  <*> argument text
      ( metavar "COMMAND"
      <> help "Command to forward to docker-compose")
  <*> many (argument text
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
