module Main where

import Data.List (isInfixOf)
import System.FilePath ((</>))
import System.Directory (listDirectory)


getFullPaths :: IO [FilePath]
getFullPaths = do
  let dir = "haskell/src"
  names <- listDirectory dir
  let matches = filter (isInfixOf "Interface") names
  return $ map (dir </>) matches


main :: IO ()
main = do 
  files <- getFullPaths

  putStrLn "\n=================================================================================================="
  putStrLn "                                   Haskell SDP Framework"
  putStrLn "==================================================================================================\n"

  putStrLn "This interface allows you to load and run various SDP models interactively via GHCi.\n"

  putStrLn "Available Interface Modules:"
  putStrLn "----------------------------------------------------------------------------------"
  mapM_ (\f -> putStrLn $ "  • " ++ f) files
  putStrLn "----------------------------------------------------------------------------------\n"

  putStrLn "How to Get Started:"
  putStrLn "----------------------------------------------------------------------------------"
  putStrLn "  1. Load the desired interface module in GHCi:"
  mapM_ (\f -> putStrLn $ "       > :l " ++ f) files
  putStrLn "\n  2. Type 'help' to see usage instructions and available functions."
  putStrLn "  3. To exit GHCi, simply type ':q'\n"

  putStrLn "Notes:"
  putStrLn "  • This tool is designed for experimentation, policy analysis, and decision modeling."
  putStrLn "  • All outputs are typed and deterministic based on your input configuration."
  putStrLn "  • Source code is modular and extensible – feel free to explore and modify!\n"

  putStrLn "=================================================================================================="
