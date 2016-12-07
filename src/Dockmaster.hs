{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster
  ( dm
  ) where

import Data.Either
import Data.Monoid ((<>))

-- Local modules
import Dockmaster.Locator
import Dockmaster.Parser
import Dockmaster.Types

-- External modules
import Shelly
import Text.Regex.Posix
import Prelude hiding (FilePath)
import qualified Data.Text as T
default (T.Text)

type DCCommand = T.Text

-- | Runs docker-compose commands against resolved composition locations
-- See usage docs for more info. Tries to find a dockmaster.yml file based on
-- the initial path argument
-- TODO monad >>= and >> the shit out of this to remove the casing structure
-- REMEMBER you can do `when weAreDone exitSuccess` as control flow in do statement
dm :: FilePath -> DCCommand -> [T.Text] -> Sh ()
dm path command args = do
  eWd <- getWorkDir path
  case eWd of
    Left err -> errorExit err
    Right wd -> sub $ do
      cd wd
      dmYml <- dockmasterYml
      case dmYml of
        Left err    -> echo_err err
        Right dmYml -> do
          let targets = map targetName $ dmTargets dmYml -- primitively just grabbing machine name
          results <- (flip mapM) targets $ \m -> dockermachine m $ do
            response <- hookWrap command $ dockercompose $ command : args
            echo response
          mapM_ echo_err $ lefts results


-- | Run docker-compose command
dockercompose :: [T.Text] -> Sh T.Text
dockercompose = run "docker-compose"

-- | Executes specific dc command pre/post hooks around action argument
-- (action arg is typically docker-compose command)
hookWrap :: DCCommand -> Sh a -> Sh a
hookWrap = undefined

-- | Takes machine name and Sh action, and wraps Sh action in scope of
-- docker-machine env
dockermachine :: T.Text -> Sh a -> Sh (Either T.Text a)
dockermachine m action = sub $ do
  envvars <- run "docker-machine" ["env", m]
  mapM (\(var,val) -> setenv (T.pack var) (T.pack val))
    $ pairEnvvars $ T.unpack envvars
  action >>= return . Right

-- | Accept a string of possibly many export VAR=VAL statements
-- and return the (VAR,VAL) pairs
-- TODO: Use a parser (idiomatic haskell) over regex !!!
pairEnvvars :: String -> [(String,String)]
pairEnvvars ls = foldr go [] $ lines ls
  where go :: String -> [(String,String)] -> [(String,String)]
        go l pairs = case l =~ exportPattern :: [[String]] of
                          [[_,var,val]] -> (var, val) : pairs
                          _             -> pairs

-- | Regex pattern for matching docker-machine env output
exportPattern :: String
exportPattern = "export ([A-Za-z0-9_]+)=\"(.*)\""

