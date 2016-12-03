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
import qualified Data.Text as T

default (T.Text)
-- TODO possibly put this in another module
-- CLI Flags
data CLI = CLI
  { cliCompositionDir :: String
  , cliDcCommand      :: String
  , cliDcOpts         :: String
  } deriving (Eq,Ord,Show)

runtime :: CLI -> IO T.Text
runtime opts = shelly $ verbosely $ do
  let (path, command, optargs) = mapTriple T.pack
                               ( cliCompositionDir opts
                               , cliDcCommand opts
                               , cliDcOpts opts
                               )
   in dm (fromText path) command [optargs]

parser :: Parser CLI
parser = CLI
  <$> strOption
      ( long "composition"
      <> short 'c'
      <> metavar "PATH"
      <> showDefault
      <> value "."
      <> help "Composition directory. Note this can be relative to DM_COMPOSITIONS_DIR array." )
  <*> argument str (metavar "COMMAND")
  <*> argument str (metavar "[args..]")

main :: IO T.Text
main = execParser opts >>= runtime
  where opts = info (helper <*> parser) 
          (  fullDesc
          <> progDesc "Orchestrate your docker-compose"
          <> header "dm - yaml loving docker compose orchestration"
          )

mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (x,y,z) = (f x, f y, f z)
