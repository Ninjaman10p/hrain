{-# LANGUAGE OverloadedStrings #-}
module Presets
  ( rainSim
  , windyRainSim
  , snowSim
  , snowstormSim
  , matrixSim
  , rainbowSim
  , dwarfSim
  , catsDogsSim
  , knifeSim
  , dolphinSim
  ) where

import Sim
import Graphics.Vty as V
import Brick
  ( fg
  , on
  , attrMap
  )
import Data.Map as M

rainSim :: RainSim
rainSim = RainSim
  { rainLayers =
      [ RainLayer "rainfg" 10 $ M.empty
      , RainLayer "rainbg" 3 $ M.empty
      , RainLayer "rainb" 3 $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rainbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rainb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = ",.'"
  , rainVel = Vel $ Pos (-1) 1
  }

windyRainSim :: RainSim
windyRainSim = RainSim
  { rainLayers =
      [ RainLayer "rainfg" 10 $ M.empty
      , RainLayer "rainbg" 3 $ M.empty
      , RainLayer "rainb" 3 $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rainbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rainb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = ",.'"
  , rainVel = Vel $ Pos (-2) 1
  }

snowSim :: RainSim
snowSim = RainSim
  { rainLayers =
      [ RainLayer "snowfg" 10 $ M.empty
      , RainLayer "snowbg" 3 $ M.empty
      , RainLayer "snowb" 3 $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("snowfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("snowbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("snowb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "*#❄"
  , rainVel = Vel $ Pos 1 1
  }

snowstormSim :: RainSim
snowstormSim = RainSim
  { rainLayers =
      [ RainLayer "snowfg" 10 $ M.empty
      , RainLayer "snowbg" 3 $ M.empty
      , RainLayer "snowb" 3 $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("snowfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("snowbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("snowb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "*#❄"
  , rainVel = Vel $ Pos (-3) 1
  }

matrixSim :: RainSim
matrixSim = RainSim
  { rainLayers =
      [ RainLayer "matrixfg" 10 $ M.empty
      , RainLayer "matrixbg" 3 $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("matrixfg", withStyles [] $ V.green `on` V.black)
      , ("matrixbg", withStyles [V.dim] $ V.green `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890/:?,.;☺"
  , rainVel = Vel $ Pos 0 1
  }

rainbowSim :: RainSim
rainbowSim = RainSim
  { rainLayers =
      [ RainLayer "red"     3 $ M.empty
      , RainLayer "green"   3 $ M.empty
      , RainLayer "blue"    3 $ M.empty
      , RainLayer "magenta" 3 $ M.empty
      , RainLayer "yellow"  3 $ M.empty
      , RainLayer "cyan"    3 $ M.empty
      , RainLayer "bred"     3 $ M.empty
      , RainLayer "bgreen"   3 $ M.empty
      , RainLayer "bblue"    3 $ M.empty
      , RainLayer "bmagenta" 3 $ M.empty
      , RainLayer "byellow"  3 $ M.empty
      , RainLayer "bcyan"    3 $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("red",     fg V.red)
      , ("green",   fg V.green)
      , ("blue",    fg V.blue)
      , ("magenta", fg V.magenta)
      , ("yellow",  fg V.yellow)
      , ("cyan",    fg V.cyan)
      , ("bred",     withStyles [V.bold] $ fg V.red)
      , ("bgreen",   withStyles [V.bold] $ fg V.green)
      , ("bblue",    withStyles [V.bold] $ fg V.blue)
      , ("bmagenta", withStyles [V.bold] $ fg V.magenta)
      , ("byellow",  withStyles [V.bold] $ fg V.yellow)
      , ("bcyan",    withStyles [V.bold] $ fg V.cyan)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "*"
  , rainVel = Vel $ Pos 1 1
  }
  
dwarfSim :: RainSim
dwarfSim = RainSim
  { rainLayers =
      [ RainLayer "rainfg" 10 $ M.empty
      , RainLayer "rainbg" 3 $ M.empty
      , RainLayer "rainb" 3 $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rainbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rainb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "☺"    -- It's raining dwarves
  , rainVel = Vel $ Pos 1 1
  }

catsDogsSim :: RainSim
catsDogsSim = RainSim
  { rainLayers =
      [ RainLayer "rainfg" 10 $ M.empty
      , RainLayer "rainbg" 3 $ M.empty
      , RainLayer "rainb" 3 $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rainbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rainb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = ",.'🐱🐶" -- It's raining cat's and dogs
  , rainVel = Vel $ Pos 1 1
  }

knifeSim :: RainSim
knifeSim = RainSim
  { rainLayers =
      [ RainLayer "rainfg" 10 $ M.empty
      , RainLayer "rainbg" 3 $ M.empty
      , RainLayer "rainb" 3 $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rainbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rainb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "🔪" -- It's raining knives
  , rainVel = Vel $ Pos 1 1
  }

dolphinSim :: RainSim
dolphinSim = RainSim
  { rainLayers =
      [ RainLayer "dolphin" 3 $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("dolphin", withStyles [V.italic] $ V.blue `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "🐬" -- It's raining knives
  , rainVel = Vel $ Pos 1 (-1)
  }
