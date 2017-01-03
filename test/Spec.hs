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

robComposition :: FilePath
robComposition = fromText "test/fixtures/compositions/rob_example"

nonExistingComposition :: FilePath
nonExistingComposition = fromText "test/fixtures/compositions/NONEXISTENT"

cleanup :: IO ()
cleanup = shelly $ verbosely $ do
  rm_rf $ robComposition </> "output"

startup :: IO ()
startup = shelly $ verbosely $ do
  cd robComposition
  rm_rf "output"
  mkdir_p "output"
  touchfile $ "output" </> "favnum.txt"

main :: IO ()
main = hspec $
      beforeAll_ startup
    . afterAll_ cleanup
  $ do

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
    validWD   <- runIO $ shelly $ silently $ getWorkDir' baseConfig robComposition
    invalidWD <- runIO $ shelly $ silently $ getWorkDir' baseConfig nonExistingComposition
    it "should resolve existing compositions" $ do
      validWD `shouldBe` (Right robComposition)
    it "should fail appropriately" $ do
      invalidWD `shouldBe` (Left WorkDirNotFound)

  describe "Environment interaction" $ do
    txt <- runIO $ shelly $ verbosely $ errExit True $ do
      cd robComposition
      hookWrap "num" $ return ()
      readfile "output/favnum.txt"
    it "can propogate environment variables" $ do
      txt `shouldBe` "favorite number is 1\n"
