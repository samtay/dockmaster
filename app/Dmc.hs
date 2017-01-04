{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Options.Utils (text)
import Options.Applicative
import Shelly hiding (command)

import Dockmaster ((</>>=))
import qualified Dockmaster as D
import Data.Either.Combinators (fromLeft)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Control.Monad ((>=>))
import Prelude hiding (FilePath)
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
main = do
  dmc <- execParser opts
  shelly $ do
    runInit
    runDmc dmc

-- | Creates user configuration file in dockmaster home if it does not exist
--
-- The file is named @config.yml@ and by default is located in
-- @~/.dockmaster@ but can be overridden via @DOCKMASTER_HOME@.
runInit :: Sh ()
runInit = do
  mPath <- D.resolvePath
  cPath <- D.getDmHomeDirectory </>>= "config.yml"
  when (isJust mPath) $ do
    let (Just p) = mPath
    when (p /= cPath) $ return undefined

-- | Runtime dmc execution
runDmc :: Dmc -> Sh ()
runDmc (Set     (SetOptions n v)) = runSave n v
runDmc (Get     (GetOptions n))   = runGet n
runDmc (Unshift (SetOptions n v)) = runUnshift n v
runDmc (Shift   (GetOptions n))   = runShift n
runDmc (Push    (SetOptions n v)) = runPush n v
runDmc (Pop     (GetOptions n))   = runPop n

runSave = undefined
runGet = undefined
runUnshift = undefined
runShift = undefined
runPush = undefined
runPop = undefined

-- | Parser for /set/ commands
setOptions :: ReadM T.Text -> Parser SetOptions
setOptions optType = SetOptions
  <$> argument optType (metavar "NAME" <> help "Name of the setting to modify")
  <*> argument text (metavar "VALUE" <> help "Value to add/set")

-- | Parser for /get/ commands
getOptions :: Parser GetOptions
getOptions = GetOptions
  <$> argument anyfield (metavar "NAME" <> help "Name of the setting to retrieve")

-- | Reader for any config fields
anyfield :: ReadM T.Text
anyfield = eitherReader inConfig

-- | Reader for array config fields
arrfield :: ReadM T.Text
arrfield = eitherReader inConfigArr

-- | Parse argument for any value type
inConfig :: String -> Either String T.Text
inConfig field
  | field `elem` validFields = Right $ T.pack field
  | otherwise                = Left $ field ++ " is not a valid config field."

-- | Parse argument for array value type
inConfigArr :: String -> Either String T.Text
inConfigArr = inConfig >=> (isArr . T.unpack)
  where
    isArr field
      | field `elem` validArrFields = Right $ T.pack field
      | otherwise                   = Left $ field ++ " is not an array type."

-- | Valid fields
validFields :: [String]
validFields = map T.unpack D.configFields

-- | Valid array fields
validArrFields :: [String]
validArrFields = map T.unpack D.arrayFields

-- | Parser for 'Dmc'.
parser :: Parser Dmc
parser = subparser
  (
     (command "set" $ commandInfo
       (Set <$> setOptions anyfield)
       "Set value")
  <> (command "get" $ commandInfo
       (Get <$> getOptions)
       "Get value")
  <> (command "unshift" $ commandInfo
       (Unshift <$> setOptions arrfield)
       "Unshift value (for arrays)")
  <> (command "shift" $ commandInfo
       (Shift <$> getOptions)
       "Shift value (for arrays)")
  <> (command "push" $ commandInfo
       (Push <$> setOptions arrfield)
       "Push value (for arrays)")
  <> (command "pop" $ commandInfo
       (Pop <$> getOptions)
       "Pop value (for arrays)")
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

