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
import Data.Semigroup ((<>))
import qualified Data.Text as T

default (T.Text)

main :: IO T.Text
main = shelly $ verbosely $ do
  args <- liftIO $ getArgs
  case map T.pack args of
    (path : command : optargs) -> dm (fromText path) command optargs
    otherwise                  -> echo_n_err usage >> return usage

usage :: T.Text
usage =
  "\
    \\n Dockmaster is yaml loving docker compose orchestration\
    \\n\
    \\n Usage: dm PATH COMMAND [arg...]\
    \\n\
    \\nAvailable options:\
    \\n  none at the moment\
    \\n\
    \\nAvailable commands:\
    \\n  dm PATH COMMAND [arg..]     Executes 'docker-compose COMMAND [args..]'\
    \\n                              in the resolved composition directory from PATH.\
    \\n                              Note that if [args..] are present, the -- operator\
    \\n                              is necessary, i.e. dm . -- up -d"
