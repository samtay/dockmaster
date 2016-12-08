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
