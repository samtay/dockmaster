{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

-- Local packages
import Dockmaster
import Options.Utils

-- External packages
import Shelly hiding (command)
import Options.Applicative

-- Base packages
import Prelude hiding (FilePath)
import Data.Monoid ((<>))
import qualified Data.Text as T

default (T.Text)

-- | Represents cli options/arguments
data Dmc
  = Set     SetOptions
  | Get     GetOptions
  | Unshift SetOptions
  | Shift   GetOptions
  | Push    SetOptions
  | Pop     GetOptions
  deriving (Eq, Show)

-- | Arguments necessary for setting value
data SetOptions = SetOptions
  { sName  :: String
  , sValue :: String
  } deriving (Eq, Show)

-- | Arguments necessary for getting value or shifting/popping array
data GetOptions = GetOptions
  { gName  :: String
  } deriving (Eq, Show)

-- | Main runtime
main :: IO ()
main = execParser opts >>= shelly . runtime 

-- | Runtime shell execution
runtime :: Dmc -> Sh ()
runtime opts = undefined

-- | Parser for /set/ commands
setOptions :: Parser SetOptions
setOptions = SetOptions
  <$> argument str (metavar "NAME" <> help "Name of the setting to modify")
  <*> argument str (metavar "VALUE" <> help "Value to add/set")

-- | Parser for /get/ commands
getOptions :: Parser GetOptions
getOptions = GetOptions
  <$> argument str (metavar "NAME" <> help "Name of the setting to retrieve")

-- | Parser for 'Dmc'.
parser :: Parser Dmc
parser = subparser
  (  (command "set" $ commandInfo (Set <$> setOptions) "Set value")
  <> (command "get" $ commandInfo (Get <$> getOptions) "Get value")
  <> (command "unshift" $ commandInfo (Unshift <$> setOptions) "Unshift value (for arrays)")
  <> (command "shift" $ commandInfo (Shift <$> getOptions) "Shift value (for arrays)")
  <> (command "push" $ commandInfo (Push <$> setOptions) "Push value (for arrays)")
  <> (command "pop" $ commandInfo (Pop <$> getOptions) "Pop value (for arrays)")
  ) 

-- | Generate 'ParserInfo' for 'Dmc'.
opts :: ParserInfo Dmc
opts = info (helper <*> parser)
  (  fullDesc
  <> header "dmc - dockmaster configuration modifiers"
  )

-- | Helper to generate subcommand parser info
commandInfo :: Parser Dmc -> String -> ParserInfo Dmc
commandInfo opts desc = info
  (helper <*> opts)
  (fullDesc <> progDesc desc)
