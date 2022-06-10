{-# LANGUAGE OverloadedStrings #-}

module Main where

import Sim
import Presets
import System.Environment (getArgs)
import qualified Data.Map as M

main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> mkSim rainSim
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
