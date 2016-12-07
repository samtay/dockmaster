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

-- TODO possibly put this in another module
-- CLI Flags
data CLI = CLI
  { cliCompositionDir :: String
  , cliDcCommand      :: String
  , cliDcOpts         :: [String]
  } deriving (Eq,Ord,Show)

runtime :: CLI -> IO ()
runtime opts = shelly $ silently $ do
  let (path, command, optargs)
        = ( T.pack $ cliCompositionDir opts
          , T.pack $ cliDcCommand opts
          , map T.pack $ cliDcOpts opts
          )
   in dm (fromText path) command optargs

parser :: Parser CLI
parser = CLI
  <$> strOption
      ( long "composition"
      <> short 'c'
      <> metavar "PATH"
      <> showDefault
      <> value "."
      <> help "Composition directory. Note this can be relative to DM_COMPOSITIONS_DIR array."
      )
  <*> argument str (metavar "COMMAND")
  <*> many (argument str (metavar "args"))

main :: IO ()
main = execParser opts >>= runtime
  where opts = info (helper <*> parser) 
          (  fullDesc
          <> progDesc "Orchestrate your docker-compose"
          <> header "dm - yaml loving docker compose orchestration"
          )
