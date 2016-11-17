module Main where

import Data.Yaml
import Dockmaster.Types
import qualified Data.ByteString as BS
import Data.Maybe
import System.Exit
import System.Directory

parseDockmasterYml :: FilePath -> IO Bool
parseDockmasterYml "."  = return True
parseDockmasterYml ".." = return True
parseDockmasterYml file = do
  contents <- BS.readFile $ "./test/fixtures/" ++ file
  case (decodeEither contents :: Either String Dockmaster) of
       (Left e)   -> putStrLn ("Failed to parse " ++ file ++":") >> putStrLn e >> return False
       otherwise  -> putStrLn ("Parsed " ++ file ++ " successfully.") >> return True

main :: IO ()
main = do
  files   <- getDirectoryContents "./test/fixtures/"
  results <- mapM parseDockmasterYml files
  if and results
     then exitWith ExitSuccess
     else exitWith (ExitFailure 1)
