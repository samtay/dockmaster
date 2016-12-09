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

  -- * Re-exported for convenience
  , module Dockmaster.Config.Types
  ) where

-- Local modules
import Dockmaster.Config.Types
import Dockmaster.Utils (eitherWrap, testM)

-- External modules
import Data.Yaml
import qualified Data.ByteString as BS
import Shelly
import Prelude hiding (FilePath)
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP
import qualified Filesystem as F
import Data.Monoid ((<>), mconcat, First(..))
import Control.Monad (liftM, liftM2)
default (T.Text)

---------- dm config functions ----------

-- | Get global dockmaster config
--
-- If config.yml fails to parse, returns Left error.
-- If config.yml is not found, returns Right baseConfig (default configuration).
config :: Sh (Either T.Text Config)
config = do
  mPath <- resolvePath
  case mPath of
    Nothing     -> return $ Right baseConfig
    (Just path) -> do
      contents <- readBinary path
      return $
        eitherWrap T.pack id (decodeEither contents :: Either String Config)

-- | Get base config options
baseConfig :: Config
baseConfig = Config { dmcPaths = [] }

-- | Resolves path to dockmaster config.yml in the following order of precedence:
--
--     (1) DOCKMASTER_CONFIG environment variable
--     (2) $HOME/.dockmaster/config.yml
--     (3) /etc/dockmaster/config.yml
resolvePath :: Sh (Maybe FilePath)
resolvePath = do
  envPathT  <- get_env "DOCKMASTER_CONFIG"
  homePath  <- testM test_e $ getHomeDirectory </>>= ".dockmaster" </> "config.yml"
  etcPath   <- testM test_e $ return "/etc" </>>= "dockmaster" </> "config.yml"
  return $
    getFirst . mconcat $ map First [envPathT >>= (return . fromText), homePath, etcPath]

---------- dm workdir functions ----------

-- | Resolve the appropriate dockmaster workdir.
--
-- For example, if @$CWD/dockmaster.yml@ exists, then
-- >>> getWorkDir "."
-- Right "."
--
-- If @$CWD/dockmaster.yml@ does /not/ exist, then
-- >>> getWorkDir "."
-- Left "dockmaster.yml file not found"
--
-- This function will also try to resolve relative paths against the 'dmcPath'
-- composition listing directories, if any are specified by global config.
-- For example, if:
--
--   (1) @$HOME/git@ is a PATH specified in global config.yml
--   (2) @$HOME/git/deploybot/dockmaster.yml@ exists
--   (3) @$CWD/deploybot/dockmaster.yml@ does /not/ exist, then
-- >>> getWorkDir "deploybot"
-- Right "$HOME/git/deploybot"
--
-- TODO Use monad transformers for all Sh (Either a b) types, like a real man
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
  mPath <- getFirst <$> if FP.absolute p
              then tryPath p
              else mconcat <$> mapM tryPath (map (</> p) $ "." : dmcPaths cfg)

  return $ maybe workDirNotFound Right mPath

-- | Check if directory @dir@ contains a @dockmaster.yml@ file
-- If it does, return @First dir@, otherwise Nothing
-- Using the @First@ monoid so we can have precedence for composition listings
tryPath :: FilePath -> Sh (First FilePath)
tryPath dir = do
  found <- test_e (dir </> "dockmaster.yml")
  return . First $ if found
     then Just dir
     else Nothing


-- | Just a small abstraction to keep error message on its own
workDirNotFound :: Either T.Text b
workDirNotFound = Left "dockmaster.yml file not found"

-- | Get home directory (in Sh)
getHomeDirectory :: Sh FilePath
getHomeDirectory = liftIO F.getHomeDirectory

infixr 4 </>>=
(</>>=) :: (Monad m) => m FilePath -> FilePath -> m FilePath
mFp </>>= fp = mFp <</>> (return fp)

infixr 5 <</>>
(<</>>) :: (Monad m) => m FilePath -> m FilePath -> m FilePath
(<</>>) = liftM2 (</>)
