{-|
Module      : Options.Utils
Description : Options.Applicative utilities
License     : ASL-2
Maintainer  : sam.chong.tay@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Options.Utils where

import Options.Applicative

import Shelly (FilePath, fromText)
import Prelude hiding (FilePath)
import Data.Monoid ((<>))
import qualified Data.Text as T

default (T.Text)

-- | Custom parser to show help on error
execParser' :: ParserInfo a -> IO a
execParser' = customExecParser (prefs showHelpOnEmpty)

-- | Version option (shared between dm and dmc)
--
-- Shows version and exits
versionOption :: Parser (a -> a)
versionOption = infoOption "version @VERSION@ build @BUILD@" (long "version" <> help "Show version")

-- | 'Text' option
textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

-- | 'Text' argument type
text :: ReadM T.Text
text = fmap T.pack str

filepath :: ReadM FilePath
filepath = fmap fromText text

-- | 'FilePath' option
filePathOption :: Mod OptionFields String -> Parser FilePath
filePathOption = fmap fromText . textOption
