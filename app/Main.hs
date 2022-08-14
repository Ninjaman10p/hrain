{-# LANGUAGE OverloadedStrings #-}

module Main where

import Sim
import Presets
import System.Environment (getArgs, getProgName)
import qualified Data.Map as M

main :: IO ()
main = do
  args <- getArgs
  progname <- getProgName
  case args of
    []    -> mkSim rainSim
    ("--help":_) -> putStrLn $ helpMessage progname
    (a:_) -> case M.lookup a sims of
      (Just sim) -> mkSim sim
      Nothing    -> putStrLn "ERROR: Simulation not found"

sims :: M.Map String RainSim
sims = M.fromList
  [ ("rain", rainSim)
  , ("storm", stormSim)
  , ("snow", snowSim)
  , ("snowstorm", snowstormSim)
  , ("rainbow", rainbowSim)
  , ("matrix", matrixSim)
  , ("dwarf", dwarfSim)
  , ("cats-dogs", catsDogsSim)
  , ("knife", knifeSim)
  , ("thanks-fish", dolphinSim)
  ]
  
helpMessage :: String -> String
helpMessage progname = unlines $
  [ "USAGE: " <> progname <> " [SIMULATION]"
  , "       " <> progname <> " --help (to display this message)"
  , ""
  , "SIMULATIONS: (default: rain)"
  ] <> (("    " <>) <$> M.keys sims)

