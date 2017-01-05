{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Options.Applicative
import Shelly hiding (command)

import Options.Utils (text, execParser')
import Dockmaster ((</>>=))
import qualified Dockmaster as D
import qualified Data.Aeson as J
import Data.Yaml ((.=))
import qualified Data.Yaml as Y
import qualified Data.HashMap.Lazy as HM
import Data.Either.Combinators (fromLeft)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.List ((\\))
import Control.Monad ((>=>), forM_)
import Prelude hiding (FilePath)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

default (T.Text)

-- | Represents cli options/arguments
data Dmc
  = Set     SetOptions
  | Get     GetOptions
  | Unshift SetOptions
  | Shift   GetOptions
  | Push    SetOptions
  | Pop     GetOptions
  | Cat
  | Ls
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
  dmc <- execParser' opts
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
  cPath <- configFP
  -- Dont overwrite existing config file
  unlessM (test_e cPath) $
    case mPath of
      -- Put empty config file in dm home
      Nothing -> save D.baseConfig
      -- Copy default config file to dm home
      Just p -> unless (p == cPath) $ cp p cPath

-- | Runtime dmc execution
runDmc :: Dmc -> Sh ()
runDmc (Set     (SetOptions n v)) = runSet n v
runDmc (Get     (GetOptions n))   = runGet n
runDmc (Unshift (SetOptions n v)) = runUnshift n v
runDmc (Shift   (GetOptions n))   = runShift n
runDmc (Push    (SetOptions n v)) = runPush n v
runDmc (Pop     (GetOptions n))   = runPop n
runDmc Cat                        = runCat
runDmc Ls                         = runLs

-- | Set value
runSet :: T.Text -> T.Text -> Sh ()
runSet n v = do
  cfg <- configO
  let newCfg = HM.fromList [(n, read' n v)] <> cfg
  case J.fromJSON (Y.Object newCfg) of
    J.Error err   -> do
      echo_err "Could not convert to valid configuration"
      D.errorExit' (T.pack err)
    J.Success cfg -> do
      save cfg
      echo "Saved successfully"
      exit 0

-- | Get value
runGet :: T.Text -> Sh ()
runGet n = do
  cfgO <- configO
  case HM.lookup n cfgO of
    Nothing  -> D.errorExit' $ n `T.append` " field not found"
    (Just v) -> echo (show' v) >> exit 0

runUnshift = undefined
runShift = undefined
runPush = undefined
runPop = undefined

-- | Print the full current configuration yaml
runCat :: Sh ()
runCat = (show' . Y.Object <$> configO) >>= echo

-- | List all available fields, broken down by value type
runLs :: Sh ()
runLs = do
  let flatFields = D.configFields \\ D.arrayFields
      fieldInfo = [("Flat", flatFields), ("Array", D.arrayFields)]
  forM_ fieldInfo $
    \(n, fs) -> unless (null fs) $ do
      echo $ n `T.append` " config fields:"
      mapM_ echo fs
      echo ""

-- | Save a 'Config' instance to user config file
save :: D.Config -> Sh ()
save cfg = do
  cPath <- configFP
  writeBinary cPath $ Y.encode cfg

-- | Get path to dockmaster home config file
configFP :: Sh FilePath
configFP = D.getDmHomeDirectory </>>= "config.yml"

-- | Get config from dockmaster home
config :: Sh D.Config
config = do
  contents <- configFP >>= readBinary
  case Y.decodeEither contents :: Either String D.Config of
    Left err  -> do
      echo_err $ T.unlines [decodeErrorMsg, T.pack err]
      quietExit 1
    Right cfg -> return cfg

-- | Get config as aeson object from dockmaster home
configO :: Sh Y.Object
configO = do
  val <- fmap Y.toJSON config
  case val of
    Y.Object obj -> return obj
    _            -> echo_err decodeErrorMsg >> exit 1

-- | Read cli values that get marshalled into config yaml
read' :: T.Text -> T.Text -> Y.Value
read' n v
  | n `elem` D.arrayFields = Y.toJSON $ filter (/= T.empty) $ T.splitOn ":" v
  | otherwise              = Y.toJSON v

-- | Show config yaml values to in cli friendly format
show' :: Y.Value -> T.Text
show' = T.decodeUtf8 . Y.encode

-- | Parser for /set/ commands
setOptions :: ReadM T.Text -> Parser SetOptions
setOptions optType = SetOptions
  <$> argument optType (metavar "NAME" <> help "Name of the setting to modify")
  <*> argument text (metavar "VALUE" <> help "Value to add/set")

-- | Parser for /get/ commands
getOptions :: ReadM T.Text -> Parser GetOptions
getOptions optType = GetOptions
  <$> argument optType (metavar "NAME" <> help "Name of the setting to retrieve")

-- | Reader for any config fields
anyfield :: ReadM T.Text
anyfield = text >>= inConfig

-- | Reader for array config fields
arrfield :: ReadM T.Text
arrfield = text >>= inConfig >>= inConfigArr

-- | Parse argument for any value type
inConfig :: T.Text -> ReadM T.Text
inConfig field
  | field `elem` D.configFields = return field
  | otherwise                   = readerError $ (T.unpack field) ++ " is not a valid config field. "
                                    ++ "Try the 'ls' command for help."

-- | Parse argument for array value type
inConfigArr :: T.Text -> ReadM T.Text
inConfigArr field
  | field `elem` D.arrayFields = return field
  | otherwise                  = readerError $ (T.unpack field) ++ " is not an array type."
                                   ++ "Try the 'ls' command for help."

-- | Parser for 'Dmc'.
parser :: Parser Dmc
parser = subparser
  (
     (command "set" $ commandInfo
       (Set <$> setOptions anyfield)
       "Set value")
  <> (command "get" $ commandInfo
       (Get <$> getOptions anyfield)
       "Get value")
  <> (command "unshift" $ commandInfo
       (Unshift <$> setOptions arrfield)
       "Unshift value (for arrays)")
  <> (command "shift" $ commandInfo
       (Shift <$> getOptions arrfield)
       "Shift value (for arrays)")
  <> (command "push" $ commandInfo
       (Push <$> setOptions arrfield)
       "Push value (for arrays)")
  <> (command "pop" $ commandInfo
       (Pop <$> getOptions arrfield)
       "Pop value (for arrays)")
  <> (command "cat" $ commandInfo
       (pure Cat)
       ("Cat full configuration"))
  <> (command "ls" $ commandInfo
       (pure Ls)
       ("List available config fields"))
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

decodeErrorMsg :: T.Text
decodeErrorMsg = "Current configuration is invalid. Please delete the config file "
                   `T.append` "and try again."

