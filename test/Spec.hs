{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Dockmaster

import Test.Hspec
import Shelly
import Prelude hiding (FilePath)
import Data.Either
import Control.Monad
import qualified Data.Text as T
default (T.Text)

validFiles :: Sh [FilePath]
validFiles = ls $ fromText "test/fixtures/valid"

invalidFiles :: Sh [FilePath]
invalidFiles = ls $ fromText "test/fixtures/invalid"

existingComposition :: FilePath
existingComposition = fromText "test/fixtures/compositions/exists"

nonExistingComposition :: FilePath
nonExistingComposition = fromText "test/fixtures/compositions/NONEXISTENT"

main :: IO ()
main = hspec $ do

  describe "Parsing dockmaster.yml files" $ do
    validFiles   <- runIO $ shelly $ validFiles >>= mapM parseYml 
    invalidFiles <- runIO $ shelly $ invalidFiles >>= mapM parseYml
    forM_ validFiles $ \file -> do
      it "should parse valid files" $ do
        file `shouldSatisfy` isRight
    forM_ invalidFiles $ \file -> do
      it "should fail appropriately" $ do
        file `shouldSatisfy` isLeft

  describe "Relative workdir resolution" $ do
    validWD   <- runIO $ shelly $ silently $ getWorkDir' baseConfig existingComposition
    invalidWD <- runIO $ shelly $ silently $ getWorkDir' baseConfig nonExistingComposition
    it "should resolve existing compositions" $ do
      validWD `shouldBe` (Right existingComposition)
    it "should fail appropriately" $ do
      invalidWD `shouldBe` (Left WorkDirNotFound)

  describe "Environment interaction" $ do
    it "should successfully execute locally" $ do
      pendingWith "SAM needs to add --local flag so we can test dm"
    it "can propogate environment variables" $ do
      pendingWith "SAM needs to add --local flag so we can test dm"

-- After adding --local flag and testing dm execution,
-- we can then test if env variables were written to output dir correctly,
-- and then do an hspec cleanup via "afterAll_"
