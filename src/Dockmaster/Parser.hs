{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster.Parser
  ( dockmasterYml
  -- * Dockmaster command configuration
  , hookWrap
  , hookWrap'
  -- * Re-exported for convenience
  , module Dockmaster.Types
  ) where

-- Local modules
import Dockmaster.Utils (eitherWrap)
import Dockmaster.Config.Parser
import Dockmaster.Types

-- External modules
import Data.Yaml
import qualified Data.ByteString as BS
import Data.HashMap.Lazy (HashMap, lookup, member)
import Data.Monoid ((<>), mconcat, First(..))
import Shelly
import Prelude hiding (FilePath, lookup)
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
hookWrap :: T.Text -> Sh () -> Sh ()
hookWrap dcCmd action = do
  dmYml <- dockmasterYml
  either
    errorExit                            -- Exit on dockmaster.yml parsing failure
    (\cfg -> hookWrap' cfg dcCmd action) -- Otherwise execute hooks & docker-compose
    dmYml

-- | Same thing as 'hookWrap' but accepts 'Dockmaster' config to execute against
hookWrap' :: Dockmaster -> T.Text -> Sh () -> Sh ()
hookWrap' cfg dcCmd action = do
  -- Gather command configuration hooks/pass-through
  let cmdCfg = lookup dcCmd $ dmCommands cfg
      pre    = maybe [] ccPreHooks cmdCfg
      post   = maybe [] ccPreHooks cmdCfg
      pass   = maybe True ccCompose cmdCfg
  mapM_ execHook pre  -- Exec pre hooks
  when pass action    -- Exec docker-compose (unless pass-through = false)
  mapM_ execHook post -- Exec post hooks

-- | Execute hook
execHook :: Hook -> Sh ()
-- Execute "file" hook type
execHook (File txt)  = do
  -- Need to force relative path (Shelly oddism)
  let file = fromText $ if not (T.null txt) && (T.head txt /= '/')
      then T.append "./" txt
      else txt
  bash_ file []
-- Execute "shell" hook type
execHook (Shell txt) = unless (T.null txt) $ do
  let (shCmd:shArgs) = T.words txt
  run_ (fromText shCmd) shArgs
