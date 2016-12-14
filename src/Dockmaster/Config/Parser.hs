{-|
Module      : Dockmaster.Config.Parser
Description : Parsing global/user configuration
License     : ASL-2
Maintainer  : sam.chong.tay@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster.Config.Parser
  (
  -- * Getting global config
    config
  , baseConfig

  -- * Resolving relative paths
  , getWorkDir
  , getWorkDir'
  ) where

-- Local modules
import Dockmaster.Config.Types
import Dockmaster.Utils

-- External modules
import Data.Yaml
import qualified Data.ByteString as BS
import Shelly
import Prelude hiding (FilePath, log)
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP
import qualified Filesystem as F
import Data.Monoid ((<>), mconcat, First(..))
default (T.Text)

---------- dm config functions ----------

-- | Get global dockmaster config
--
-- If @config.yml@ fails to parse, returns a @Left error@.
-- If @config.yml@ is not found, returns @Right baseConfig@ (default configuration).
config :: Sh (Either T.Text Config)
config = do
  mPath <- resolvePath
  case mPath of
    Nothing     -> return $ Right baseConfig
    (Just path) -> do
      contents <- readBinary path
      case decodeEither contents :: Either String Config of
        Left err  -> return . Left $ T.pack err
        Right cfg -> return cfg >>= parseConfig >>= return . Right

-- | Get base config options
baseConfig :: Config
baseConfig = Config { dmcPaths = [] }

-- | Resolves path to dockmaster @config.yml@ in the following order of precedence:
--
--     (1) @DOCKMASTER_CONFIG@ environment variable
--     (2) @$HOME/.dockmaster/config.yml@
--     (3) @/etc/dockmaster/config.yml@
resolvePath :: Sh (Maybe FilePath)
resolvePath = do
  envPathT  <- get_env "DOCKMASTER_CONFIG"
  homePath  <- testM test_e $ getHomeDirectory </>>= ".dockmaster" </> "config.yml"
  etcPath   <- testM test_e $ return "/etc" </>>= "dockmaster" </> "config.yml"
  return $
    getFirst . mconcat $ map First [envPathT >>= (return . fromText), homePath, etcPath]

-- | Parse the paths specified in configuration
--
-- This handles in-line evaluation of @~, $HOME, $DOCKMASTER_HOME@, etc.
parseConfig :: Config -> Sh Config
parseConfig cfg = do
  parsedPaths <- mapM (\p -> toText p >>= parsePath) (dmcPaths cfg)
  return $ Config { dmcPaths = parsedPaths }

---------- dm workdir functions ----------

-- | Resolve the appropriate dockmaster workdir.
--
-- For example, if @$CWD/dockmaster.yml@ exists, then
--
-- >>> getWorkDir "."
-- Right "."
--
-- If @$CWD/dockmaster.yml@ does /not/ exist, then
--
-- >>> getWorkDir "."
-- Left "dockmaster.yml file not found"
--
-- This function will also try to resolve relative paths against the 'dmcPaths'
-- composition listing directories, if any are specified by global config.
-- For example, if:
--
--   (1) @$HOME/git@ is a @PATH@ specified in global @config.yml@
--   (2) @$HOME\/git\/deploybot/dockmaster.yml@ exists
--   (3) @$CWD\/deploybot\/dockmaster.yml@ does /not/ exist, then
--
-- >>> getWorkDir "deploybot"
-- Right "$HOME/git/deploybot"
getWorkDir :: FilePath -> Sh (Either T.Text FilePath)
getWorkDir p = do
  eCfg <- config
  case eCfg of
    (Left err)  -> return $ Left err
    (Right cfg) -> getWorkDir' cfg p

-- | Same thing as 'getWorkDir' but uses a 'Config' argument instead of
-- resolving one.
getWorkDir' :: Config -> FilePath -> Sh (Either T.Text FilePath)
getWorkDir' cfg p = do
  -- If absolute path is given, it is the only one attempted
  path  <- toText p >>= parsePath
  mPath <- getFirst <$> if FP.absolute path
              then tryPath p
              else mconcat <$> mapM tryPath (map (</> p) $ "." : dmcPaths cfg)

  return $ maybe workDirNotFound Right mPath

-- | Check if directory @dir@ contains a @dockmaster.yml@ file
-- If it does, return @First dir@, otherwise Nothing
-- Using the @First@ monoid so we can have precedence for composition listings
tryPath :: FilePath -> Sh (First FilePath)
tryPath dir = do
  toText dir >>= \s -> log $ "Looking in directory " `T.append` s
  found <- test_e (dir </> "dockmaster.yml")
  return . First $ if found
     then Just dir
     else Nothing

-- | Just a small abstraction to keep error message on its own
workDirNotFound :: Either T.Text b
workDirNotFound = Left "dockmaster.yml file not found"
