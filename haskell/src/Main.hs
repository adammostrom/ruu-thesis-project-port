module Main where

main :: IO ()
main = do
  putStrLn "\n=================================================================================================="
  putStrLn "                                   Haskell SDP Framework"
  putStrLn "==================================================================================================\n"

  putStrLn "This interface allows you to load and run various SDP models interactively via GHCi.\n"

  putStrLn "Available Modules:"
  putStrLn "----------------------------------------------------------------------------------"
  putStrLn "  • Greenhouse Gas Emission Case (baseline model)             --> GHGCase.hs"
  putStrLn "  • Advanced GHG Case (extended climate-economy model)        --> AdvancedStates.hs"
  putStrLn "----------------------------------------------------------------------------------\n"

  putStrLn "How to Get Started:"
  putStrLn "----------------------------------------------------------------------------------"
  putStrLn "  1. Load the desired interface module in GHCi:"
  putStrLn "       > :l haskell/src/Interface.hs           -- For the standard GHG case"
  putStrLn "       > :l haskell/src/InterfaceADV.hs        -- For the advanced state model\n"
  putStrLn "  2. Type 'help' to see usage instructions and available functions."
  putStrLn "  3. To exit GHCi, simply type ':q'\n"

  putStrLn "Notes:"
  putStrLn "  • This tool is designed for experimentation, policy analysis, and decision modeling."
  putStrLn "  • All outputs are typed and deterministic based on your input configuration."
  putStrLn "  • Source code is modular and extensible – feel free to explore and modify!\n"

  putStrLn "=================================================================================================="
  