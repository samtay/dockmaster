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
import Data.Either.Combinators

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
  { sName  :: T.Text
  , sValue :: T.Text
  } deriving (Eq, Show)

-- | Arguments necessary for getting value or shifting/popping array
data GetOptions = GetOptions
  { gName  :: T.Text
  } deriving (Eq, Show)

-- | Main runtime
main :: IO ()
main = execParser opts >>= shelly . runtime 

-- | Runtime shell execution
runtime :: Dmc -> Sh ()
runtime = undefined

anyfield :: ReadM T.Text
anyfield = eitherReader inConfig

arrfield :: ReadM T.Text
arrfield = eitherReader inConfigArr

inConfig :: String -> Either String T.Text
inConfig "PATHS" = Right "PATHS"
inConfig arg     = Left $ arg ++ " is not a valid config field."

inConfigArr :: String -> Either String T.Text
inConfigArr "PATHS" = Right "PATHS"
inConfigArr arg     = Left $ fromLeft (arg ++ " is not an array type.") (inConfig arg)

-- | Parser for /set/ commands
setOptions :: ReadM T.Text -> Parser SetOptions
setOptions optType = SetOptions
  <$> argument optType (metavar "NAME" <> help "Name of the setting to modify")
  <*> argument text (metavar "VALUE" <> help "Value to add/set")

-- | Parser for /get/ commands
getOptions :: Parser GetOptions
getOptions = GetOptions
  <$> argument anyfield (metavar "NAME" <> help "Name of the setting to retrieve")

-- | Parser for 'Dmc'.
parser :: Parser Dmc
parser = subparser
  (  (command "set" $ commandInfo (Set <$> setOptions anyfield) "Set value")
  <> (command "get" $ commandInfo (Get <$> getOptions) "Get value")
  <> (command "unshift" $ commandInfo (Unshift <$> setOptions arrfield) "Unshift value (for arrays)")
  <> (command "shift" $ commandInfo (Shift <$> getOptions) "Shift value (for arrays)")
  <> (command "push" $ commandInfo (Push <$> setOptions arrfield) "Push value (for arrays)")
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
