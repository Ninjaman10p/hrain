{-# LANGUAGE OverloadedStrings #-}

module Main where

import Sim
import Presets
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                -> mkSim 10000 rainSim
    ("windy":_)       -> mkSim 10000 windyRainSim
    ("snow":_)        -> mkSim 100000 snowSim
    ("snowstorm":_)   -> mkSim 10000 snowstormSim
    ("rainbow":_)     -> mkSim 20000 rainbowSim
    ("matrix":_)      -> mkSim 100000 matrixSim
    ("dwarf":_)       -> mkSim 50000 dwarfSim
    ("cats-dogs":_)   -> mkSim 20000 catsDogsSim
    ("knife":_)       -> mkSim 10000 knifeSim
    ("thanks-fish":_) -> mkSim 100000 dolphinSim
    _                 -> mkSim 10000 rainSim
