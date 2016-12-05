{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster
  ( dm
  ) where

import Data.Either
import Data.Monoid ((<>))

-- Local modules
import Dockmaster.Locator
import Dockmaster.Parser
import Dockmaster.Types

-- External modules
import Shelly
import Prelude hiding (FilePath)
import qualified Data.Text as T
default (T.Text)

-- | Possibly provide sum type data = DcUp | DcDown | ... with to string method
type DCCommand = T.Text

-- | Runs docker-compose commands against resolved composition locations
-- See usage docs for more info. Tries to find a dockmaster.yml file based on
-- the initial path argument
dm :: FilePath -> DCCommand -> [T.Text] -> Sh (T.Text)
dm path command args = do
  gwd <- getWorkDir path
  case gwd of
    Left err -> do
                echo_n_err err
                return err -- maybe dm should return Either and executable handles printing?
    Right wd -> sub $ do
      cd wd
      dockermachine -- TODO handle Left errors
      hookWrap command $ dockercompose $ command : args

dockercompose :: [T.Text] -> Sh T.Text
dockercompose = run "docker-compose"

-- | Executes specific dc command pre/post hooks around action argument
-- (action arg is typically docker-compose command)
hookWrap :: DCCommand -> Sh a -> Sh a
hookWrap = undefined

-- | Reads from dockmaster.yml to find machine, attempts to connect
-- dm should HALT when machine is unreachable
dockermachine :: Sh (Either T.Text ())
dockermachine = undefined

