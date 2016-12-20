{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Dockmaster

import Test.Hspec
import Shelly
import Prelude hiding (FilePath)
import Data.Either
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
    it "should parse valid files" $ do
      lefts validFiles `shouldBe` []
    it "should fail appropriately" $ do
      rights invalidFiles `shouldBe` []
  describe "Relative workdir resolution" $ do
    validWD   <- runIO $ shelly $ silently $ getWorkDir' baseConfig existingComposition
    invalidWD <- runIO $ shelly $ silently $ getWorkDir' baseConfig nonExistingComposition
    it "should resolve existing compositions" $ do
      validWD `shouldBe` (Right existingComposition)
    it "should fail appropriately" $ do
      invalidWD `shouldBe` workDirNotFound
