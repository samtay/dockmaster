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

type DCCommand = T.Text

-- | Runs docker-compose commands against resolved composition locations
-- See usage docs for more info. Tries to find a dockmaster.yml file based on
-- the initial path argument
-- TODO monad >>= and >> the shit out of this to remove the casing structure
-- REMEMBER you can do `when weAreDone exitSuccess` as control flow in do statement
dm :: FilePath -> DCCommand -> [T.Text] -> Sh ()
dm path command args = do
  eWd <- getWorkDir path
  case eWd of
    Left err -> echo_n_err err
    Right wd -> sub $ do
      cd wd
      dmYml <- dockmasterYml
      case dmYml of
        Left err    -> echo_n_err err
        Right dmYml -> do
          let targets = map targetName $ dmTargets dmYml -- primitively just grabbing machine name
          results <- (flip mapM) targets $ \m -> dockermachine m $ do
            response <- hookWrap command $ dockercompose $ command : args
            echo_n response
          mapM_ echo_n_err $ lefts results


-- | Run docker-compose command
dockercompose :: [T.Text] -> Sh T.Text
dockercompose = run "docker-compose"

-- | Executes specific dc command pre/post hooks around action argument
-- (action arg is typically docker-compose command)
hookWrap :: DCCommand -> Sh a -> Sh a
hookWrap = undefined

-- | Takes machine name and Sh action, and wraps Sh action in scope of
-- docker-machine env
dockermachine :: T.Text -> Sh a -> Sh (Either T.Text a)
dockermachine m action = sub $ do
  run "docker-machine" ["env", m]
  -- TODO check if successful before running action
  if True
     then return $ Left $ "Could not reach docker machine " <> m
     else action >>= return . Right

