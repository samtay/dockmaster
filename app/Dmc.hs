{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Options.Applicative
import Shelly hiding (command)

import Options.Utils (text, execParser', versionOption)
import Dockmaster ((</>>=))
import qualified Dockmaster as D
import qualified Data.Aeson as J
import Data.Yaml ((.=))
import qualified Data.Yaml as Y
import qualified Data.HashMap.Lazy as HM
import Data.Either.Combinators (fromLeft)
import Data.Maybe (isJust)
import Data.Monoid ((<>), mappend)
import Data.List ((\\))
import Control.Monad ((>=>), forM_)
import qualified Data.Vector as V
import Prelude hiding (FilePath)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

default (T.Text)

-- | Represents cli options/arguments
data Dmc
  = Set     SetOptions
  | Get     GetOptions
  | Unshift SetOptions
  | Push    SetOptions
  | Shift   DropOptions
  | Pop     DropOptions
  | Cat
  | Ls      LsOption
  deriving (Eq, Show)

-- | Arguments necessary for modifying value
data SetOptions = SetOptions
  { sName  :: T.Text
  , sValue :: [T.Text]
  } deriving (Eq, Show)

-- | Arguments necessary for getting value
data GetOptions = GetOptions
  { gName  :: T.Text
  } deriving (Eq, Show)

-- | Arguments necessary for dropping array elements
data DropOptions = DropOptions
  { dName  :: T.Text
  , dCount :: Int
  } deriving (Eq, Show)

-- | Arguments to list command
data LsOption = Fields | Compositions
  deriving (Eq, Show)

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
      Nothing -> D.getDmHomeDirectory >>= mkdir_p >> touchfile cPath >> save D.baseConfig
      -- Copy default config file to dm home
      Just p -> unless (p == cPath) $ cp p cPath

-- | Runtime dmc execution
runDmc :: Dmc -> Sh ()
runDmc (Set     (SetOptions n v))  = runSet n v
runDmc (Get     (GetOptions n))    = runGet n
runDmc (Unshift (SetOptions n v))  = runUnshift n v
runDmc (Push    (SetOptions n v))  = runPush n v
runDmc (Shift   (DropOptions n c)) = runShift n c
runDmc (Pop     (DropOptions n c)) = runPop n c
runDmc Cat                         = runCat
runDmc (Ls x)                      = runLs x

-- | Set value
runSet :: T.Text -> [T.Text] -> Sh ()
runSet n v = do
  cfg <- configO
  val <- read' n v
  -- Overwrite (n, v)
  saveO $ HM.fromList [(n, val)] <> cfg
  recap n

-- | Get value
runGet :: T.Text -> Sh ()
runGet n = do
  cfgO <- configO
  case HM.lookup n cfgO of
    Nothing  -> D.errorExit' $ T.unwords [n, "field not found"]
    (Just v) -> echo (show' v) >> exit 0

-- | Unshift array value(s)
runUnshift :: T.Text -> [T.Text] -> Sh ()
runUnshift n v = runAddToArray mappend n v >> recap n

-- | Push array value(s)
runPush :: T.Text -> [T.Text] -> Sh ()
runPush n v = runAddToArray (flip mappend) n v >> recap n

-- | Shift array value(s)
runShift :: T.Text -> Int -> Sh ()
runShift n c = runModArray (V.drop c) n >> recap n

-- | Pop array value(s)
runPop :: T.Text -> Int -> Sh ()
runPop n c = runModArray (pop c) n >> recap n
  where pop c vec = let l = (V.length vec) - c
                     in V.take l vec

-- | Print the full current configuration yaml
runCat :: Sh ()
runCat = (show' . Y.Object <$> configO) >>= echo

-- | List entities
runLs :: LsOption -> Sh ()
-- | List all available fields, broken down by value type
runLs Fields = do
  let flatFields = D.configFields \\ D.arrayFields
      fieldInfo = [("Flat", flatFields), ("Array", D.arrayFields)]
  forM_ fieldInfo $
    \(n, fs) -> unless (null fs) $ do
      echo $ T.unwords [n, "config fields:"]
      mapM_ echo fs
      echo ""
-- | List all available compositions
runLs Compositions = config >>= D.getAvailableCompositions' >>= mapM_ echo

-- | Takes a binary operation and uses it on the argument value and current value
runAddToArray :: (Y.Array -> Y.Array -> Y.Array) -> T.Text -> [T.Text] -> Sh ()
runAddToArray addOp n v = do
  (Y.Array argV) <- read' n v
  runModArray (addOp argV) n

-- | Performs an arbitrary operation on the current array value and saves it
runModArray :: (Y.Array -> Y.Array) -> T.Text -> Sh ()
runModArray op n = do
  cfg <- configO
  let oldV = case HM.lookup n cfg of
               Just (Y.Array vec) -> vec
               _                  -> V.empty
  saveO $ HM.fromList [(n, Y.Array (op oldV))] <> cfg

-- | Save a 'Config' instance to the user config file
save :: D.Config -> Sh ()
save cfg = do
  cPath <- configFP
  writeBinary cPath $ Y.encode cfg

-- | Save an 'Object' instance to user config file
--
-- This operation only succeeds when 'Object' is succesfully decoded to a 'Config' instance.
saveO :: Y.Object -> Sh ()
saveO obj =
  case J.fromJSON (Y.Object obj) of
    J.Error err   -> do
      echo_err "Could not convert to valid configuration"
      D.errorExit' (T.pack err)
    J.Success cfg -> do
      save cfg
      echo "Saved successfully"

-- | Show the new value for a field
recap :: T.Text -> Sh ()
recap n = do
  echo $ T.unwords ["The new value for", n, "is:"]
  runGet n

-- | Get path to dockmaster home config file
configFP :: Sh FilePath
configFP = D.getDmHomeDirectory </>>= "config.yml"

-- | Get config from dockmaster home
config :: Sh D.Config
config = do
  contents <- configFP >>= readBinary
  case Y.decodeEither contents :: Either String D.Config of
    Left err  -> D.errorExit' $ T.unlines [decodeErrorMsg, T.pack err]
    Right cfg -> return cfg

-- | Get config as aeson object from dockmaster home
configO :: Sh Y.Object
configO = do
  val <- fmap Y.toJSON config
  case val of
    Y.Object obj -> return obj
    _            -> D.errorExit' decodeErrorMsg

-- | Read cli values that get marshalled into config yaml
--
-- Only array fields can have more than one value argument
read' :: T.Text -> [T.Text] -> Sh Y.Value
read' n v
  | n `elem` D.arrayFields = return $ Y.toJSON v
  | otherwise              =
    case length v of
      0 -> return $ Y.toJSON ""
      1 -> return $ Y.toJSON $ head v
      _ -> D.errorExit' "Cannot set multiple values to a non-array field"

-- | Show config yaml values to in cli friendly format
show' :: Y.Value -> T.Text
show' = T.decodeUtf8 . Y.encode

-- | Parser for /set/ commands
setOptions :: ReadM T.Text -> Parser SetOptions
setOptions optType = SetOptions
  <$> argument optType
    (  metavar "NAME"
    <> help "Name of the setting to modify"
    <> completeWith (map T.unpack D.configFields)
    )
  <*> some (argument text (metavar "VALUE" <> help ("Value(s) to add/set. "
        ++ "Hint: You can use multiple values for array types.")))

-- | Parser for /get/ commands
getOptions :: Parser GetOptions
getOptions = GetOptions
  <$> argument anyfield
    (  metavar "NAME"
    <> help "Name of the setting to retrieve"
    <> completeWith (map T.unpack D.configFields)
    )

-- | Parser for /drop/ commands
dropOptions :: Parser DropOptions
dropOptions = DropOptions
  <$> argument arrfield
    (  metavar "NAME"
    <> help "Name of the array setting to modify"
    <> completeWith (map T.unpack D.arrayFields)
    )
  <*> argument auto (metavar "COUNT" <> value 1 <> showDefault <> help "Number of values to drop")

-- | Parser for /ls/ command
lsOptions :: Parser LsOption
lsOptions = argument lsarg
  (  metavar "ENTITY"
  <> value Compositions
  <> help "Entity to list (f|fields, c|compositions)"
  <> completeWith ["fields", "compositions"]
  )

-- | Reader for any config fields
anyfield :: ReadM T.Text
anyfield = text >>= inConfig

-- | Reader for array config fields
arrfield :: ReadM T.Text
arrfield = text >>= inConfig >>= inConfigArr

-- | Reader for @ls@ arguments
lsarg :: ReadM LsOption
lsarg = text >>= fn where
  fn s
    | s `elem` ["f", "field", "fields"]                     = return Fields
    | s `elem` ["c", "comp", "composition", "compositions"] = return Compositions
    | otherwise = readerError $ unwords
      [ (T.unpack s)
      , "is not a valid entity to list. The available entities are:"
      , "fields, compositions." ]

-- | Parse argument for any value type
inConfig :: T.Text -> ReadM T.Text
inConfig field
  | field `elem` D.configFields = return field
  | otherwise                   = readerError $ unwords
                                    [ (T.unpack field)
                                    , "is not a valid config field."
                                    , "Try the 'ls f' command for help."]

-- | Parse argument for array value type
inConfigArr :: T.Text -> ReadM T.Text
inConfigArr field
  | field `elem` D.arrayFields = return field
  | otherwise                  = readerError $ unwords
                                   [ (T.unpack field)
                                   , "is not an array type."
                                   , "Try the 'ls f' command for help."]

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
  <> (command "push" $ commandInfo
       (Push <$> setOptions arrfield)
       "Push value (for arrays)")
  <> (command "shift" $ commandInfo
       (Shift <$> dropOptions)
       "Shift value (for arrays)")
  <> (command "pop" $ commandInfo
       (Pop <$> dropOptions)
       "Pop value (for arrays)")
  <> (command "cat" $ commandInfo
       (pure Cat)
       "Cat full configuration")
  <> (command "ls" $ commandInfo
       (Ls <$> lsOptions)
       "List entities")
  )

-- | Generate 'ParserInfo' for 'Dmc'.
opts :: ParserInfo Dmc
opts = info (helper <*> versionOption <*> parser)
  (  fullDesc
  <> header "dmc - dockmaster configuration modifiers"
  )

-- | Helper to generate subcommand parser info
commandInfo :: Parser Dmc -> String -> ParserInfo Dmc
commandInfo opts desc = info
  (helper <*> opts)
  (fullDesc <> progDesc desc)

decodeErrorMsg :: T.Text
decodeErrorMsg = T.unwords
  [ "Current configuration is invalid. Please delete the config file"
  , "and try again." ]

