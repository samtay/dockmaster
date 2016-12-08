{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster.Parser
  (
  dockmasterYml
  ) where

-- Local modules
import Dockmaster.Utils (eitherWrap)

-- External modules
import Data.Yaml
import Dockmaster.Types
import qualified Data.ByteString as BS

import Shelly
import Prelude hiding (FilePath)
import qualified Data.Text as T
default (T.Text)

dockmasterYml :: Sh (Either T.Text Dockmaster)
dockmasterYml = do
  contents <- readBinary "dockmaster.yml"
  return $
    eitherWrap T.pack id (decodeEither contents :: Either String Dockmaster)
