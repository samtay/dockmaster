{-|
Module      : Dockmaster.Parser
Description : Parsing dockmaster.yml and utilities around its content
License     : ASL-2
Maintainer  : sam.chong.tay@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster.Parser
  ( dockmasterYml
  -- * Dockmaster command configuration
  , prepareEnv
  , dockermachine
  , hookWrap
  , hookWrap'
  ) where

-- Local modules
import Dockmaster.Config.Parser
import Dockmaster.Utils
import Dockmaster.Types

-- External modules
import Data.Yaml
import Data.Maybe
import Control.Monad ((<=<))
import qualified Data.ByteString as BS
import Data.HashMap.Lazy (HashMap, lookup, member, toList)
import Data.Either.Combinators (mapBoth)
import Data.Monoid ((<>), mconcat, First(..))
import Text.Regex.Posix
import Shelly
import Prelude hiding (FilePath, lookup)
import qualified Data.Text as T
default (T.Text)

-- | Parse @$CWD/dockmaster.yml@
--
-- Note this assumes we are already in the correct dockmaster workdir.
dockmasterYml :: Sh (Either T.Text Dockmaster)
dockmasterYml = do
  contents <- readBinary "dockmaster.yml"
  return $
    mapBoth T.pack id (decodeEither contents :: Either String Dockmaster)

-- | Sets environment variables defined by dockmaster configuration
prepareEnv :: Dockmaster -> Sh ()
prepareEnv cfg = do
  let directVars = map (map2 T.unpack) $ toList $ ecVars $ dmEnv cfg
  parsedVars <- parseEnvFiles $ ecFiles $ dmEnv cfg
  setEnvvars $ directVars ++ parsedVars

-- | Uses COP to parse a list of files into shell vars
parseEnvFiles :: [FilePath] -> Sh [(String,String)]
parseEnvFiles fs = do
  files <- mapM parsePath' fs
  vars <- run "cop" $ "--shell" : files
  return $ pairEnvvars $ T.unpack vars

-- | Executes specific dc command pre/post hooks around action argument
-- (action arg is typically docker-compose command).
hookWrap :: T.Text -> Sh () -> Sh ()
hookWrap dcCmd action = do
  dmYml <- dockmasterYml
  either
    errorExit                            -- Exit on dockmaster.yml parsing failure
    (\cfg -> hookWrap' cfg dcCmd action) -- Otherwise execute hooks & docker-compose
    dmYml

-- | Same thing as 'hookWrap' but accepts 'Dockmaster' config to execute against.
hookWrap' :: Dockmaster -> T.Text -> Sh () -> Sh ()
hookWrap' cfg dcCmd action = do
  -- Gather command configuration hooks/pass-through
  let cmdCfg = lookup dcCmd $ dmCommands cfg
      pre    = maybe [] ccPreHooks cmdCfg
      post   = maybe [] ccPostHooks cmdCfg
      pass   = maybe True ccRunCompose cmdCfg
  mapM_ execHook pre  -- Exec pre hooks
  when pass action    -- Exec docker-compose (unless pass-through = false)
  mapM_ execHook post -- Exec post hooks

-- | Execute hook
execHook :: Hook -> Sh ()
-- Execute "file" hook type
execHook (File txt)  = do
  -- Need to force relative path (Shelly oddism)
  let file = fromText $ if not (T.null txt) && (T.head txt /= '/')
      then T.append "./" txt
      else txt
  bash_ file []
-- Execute "shell" hook type
execHook (Shell txt) = unless (T.null txt) $ do
  let (shCmd:shArgs) = T.words txt
  run_ (fromText shCmd) shArgs

-- | Takes machine name and 'Sh' action, and wraps 'Sh' action in scope of
-- @docker-machine@ env
dockermachine :: T.Text -> Sh a -> Sh a
dockermachine m action = sub $ do
  envvars <- run "docker-machine" ["env", m]
  setEnvvars $ pairEnvvars $ T.unpack envvars
  action

-- | Accept a string of possibly many export @VAR=VAL@ statements
-- and return the @(VAR,VAL)@ pairs
pairEnvvars :: String -> [(String,String)]
pairEnvvars ls = mapMaybe match $ concatMap words $ lines ls
  where match :: String -> Maybe (String,String)
        match l = case l =~ exportPattern :: [[String]] of
                    [[_,var,val]] -> Just (var, val)
                    _             -> Nothing

-- Set environment variables (from 'String') in current scope
setEnvvars :: [(String,String)] -> Sh ()
setEnvvars = mapM_ (\(var,val) -> setenv (T.pack var) (T.pack val))

-- | Regex pattern for matching @docker-machine@ env output
exportPattern :: String
exportPattern = "([A-Za-z0-9_]+)=\"(.*)\""

-- | Trivial helper to map across a tuple
map2 :: (a -> b) -> (a, a) -> (b, b)
map2 f (x,y) = (f x, f y)
