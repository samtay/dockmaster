{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster.Parser
  ( dockmasterYml
  -- * Re-exported for convenience
  , module Dockmaster.Types
  ) where

-- Local modules
import Dockmaster.Utils (eitherWrap)
import Dockmaster.Config.Parser

-- External modules
import Data.Yaml
import Dockmaster.Types
import qualified Data.ByteString as BS

import Shelly
import Prelude hiding (FilePath)
import qualified Data.Text as T
default (T.Text)

-- | Parse $CWD/dockmaster.yml
--
-- Note this assumes we are already in the correct dockmaster workdir
dockmasterYml :: Sh (Either T.Text Dockmaster)
dockmasterYml = do
  contents <- readBinary "dockmaster.yml"
  return $
    eitherWrap T.pack id (decodeEither contents :: Either String Dockmaster)

-- | Executes specific dc command pre/post hooks around action argument
-- (action arg is typically docker-compose command)
hookWrap :: T.Text -> Sh T.Text -> Sh T.Text
hookWrap dcCmd action = do
  dmYml <- dockmasterYml
  either return (\cfg -> hookWrap' cfg dcCmd action) dmYml

hookWrap' :: Dockmaster -> T.Text -> Sh a -> Sh a
hookWrap' cfg dcCmd action = do
  let cmdCfg = dmCommands cfg
  return undefined
