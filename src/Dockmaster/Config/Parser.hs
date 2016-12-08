{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster.Config.Parser
  ( config
  , baseConfig
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
import Data.Monoid ((<>), mconcat, First(..))
import Control.Monad (liftM)
default (T.Text)

-- | Get global dockmaster config
-- If config.yml fails to parse, returns Left error
-- If config.yml is not found, returns Right baseConfig (default configuration)
config :: Sh (Either T.Text Config)
config = do
  mPath <- resolvePath
  case mPath of
    Nothing     -> return $ Right baseConfig
    (Just path) -> do
      contents <- readBinary path
      return $
        eitherWrap T.pack id (decodeEither contents :: Either String Config)

-- | Resolves path to dockmaster config.yml in the following order of precedence:
-- - DOCKMASTER_CONFIG environment variable
-- - $HOME/.dockmaster/config.yml
-- - /etc/dockmaster/config.yml
resolvePath :: Sh (Maybe FilePath)
resolvePath = do
  envPathT  <- get_env "DOCKMASTER_CONFIG"
  homePath  <- testM test_e id $ "$HOME" </> ".dockmaster" </> "config.yml"
  etcPath   <- testM test_e id $ "/etc" </> "dockmaster" </> "config.yml"
  return $
    getFirst . mconcat $ map First [envPathT >>= (return . fromText), homePath, etcPath]

-- | Get base config options
baseConfig :: Config
baseConfig = Config { dmcPaths = [] }
