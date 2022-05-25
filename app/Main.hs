{-# LANGUAGE OverloadedStrings #-}

module Main where

import Sim
import Presets
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []            -> mkSim 10000 rainSim
    ("snow":_)    -> mkSim 100000 snowSim
    ("rainbow":_) -> mkSim 20000 rainbowSim
    ("matrix":_)  -> mkSim 100000 matrixSim
    _             -> mkSim 10000 rainSim
