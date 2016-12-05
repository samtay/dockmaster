{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster.Parser
  (
  dockmasterYml
  ) where

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

eitherWrap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
eitherWrap f _ (Left a)  = Left $ f a
eitherWrap _ g (Right c) = Right $ g c
