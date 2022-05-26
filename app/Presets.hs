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
      [ RainLayer "rainfg" 10 vel $ M.empty
      , RainLayer "rainbg" 3 vel $ M.empty
      , RainLayer "rainb" 3 vel $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rainbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rainb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = ",.'"
  } where
    vel = Vel $ Pos (-1) 1

windyRainSim :: RainSim
windyRainSim = RainSim
  { rainLayers =
      [ RainLayer "rainfg" 10 vel $ M.empty
      , RainLayer "rainbg" 3 vel $ M.empty
      , RainLayer "rainb" 3 vel $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rainbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rainb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = ",.'"
  } where
    vel = Vel $ Pos (-2) 1

snowSim :: RainSim
snowSim = RainSim
  { rainLayers =
      [ RainLayer "snowfg" 10 vel $ M.empty
      , RainLayer "snowbg" 3 vel $ M.empty
      , RainLayer "snowb" 3 vel $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("snowfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("snowbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("snowb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "*#❄"
  } where
    vel = Vel $ Pos 1 1

snowstormSim :: RainSim
snowstormSim = RainSim
  { rainLayers =
      [ RainLayer "snowfg" 10 vel $ M.empty
      , RainLayer "snowbg" 3 vel $ M.empty
      , RainLayer "snowb" 3 vel $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("snowfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("snowbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("snowb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "*#❄"
  } where
    vel = Vel $ Pos (-3) 1

matrixSim :: RainSim
matrixSim = RainSim
  { rainLayers =
      [ RainLayer "matrixfg" 10 vel $ M.empty
      , RainLayer "matrixbg" 3 vel $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("matrixfg", withStyles [] $ V.green `on` V.black)
      , ("matrixbg", withStyles [V.dim] $ V.green `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890/:?,.;☺"
  } where
    vel = Vel $ Pos 0 1

rainbowSim :: RainSim
rainbowSim = RainSim
  { rainLayers = do
      (color, _) <- colors
      (modif, _) <- mods
      return $ RainLayer
        { rainStyle = modif <> color
        , weighting = 3
        , rainVel = Vel $ Pos 1 1
        , rainMap = M.empty
        }
  , rainColors = attrMap (V.white `on` V.black) $ do
      (color, vColor) <- colors
      (modif, vMod) <- mods
      return $ (modif <> color, withStyles vMod $ fg vColor)
  , windowSize = Size $ Pos 20 10
  , rainReps = "*"
  } where
    colors =
      [ ("red", V.red)
      , ("green", V.green)
      , ("blue", V.blue)
      , ("magenta", V.magenta)
      , ("yellow", V.yellow)
      , ("cyan", V.cyan)
      ]
    mods =
      [ ("b", [V.bold])
      , ("", [])
      ]
  
dwarfSim :: RainSim
dwarfSim = RainSim
  { rainLayers =
      [ RainLayer "rainfg" 10 vel $ M.empty
      , RainLayer "rainbg" 3 vel $ M.empty
      , RainLayer "rainb" 3 vel $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rainbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rainb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "☺"    -- It's raining dwarves
  } where
    vel = Vel $ Pos 1 1

catsDogsSim :: RainSim
catsDogsSim = RainSim
  { rainLayers =
      [ RainLayer "rainfg" 10 vel $ M.empty
      , RainLayer "rainbg" 3 vel $ M.empty
      , RainLayer "rainb" 3 vel $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rainbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rainb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = ",.'🐱🐶"
  } where
    vel = Vel $ Pos 1 1

knifeSim :: RainSim
knifeSim = RainSim
  { rainLayers =
      [ RainLayer "rainfg" 10 vel $ M.empty
      , RainLayer "rainbg" 3 vel $ M.empty
      , RainLayer "rainb" 3 vel $ M.empty
      ]
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rainbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rainb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , rainReps = "🔪"
  } where
    vel = Vel $ Pos 1 1

dolphinSim :: RainSim
dolphinSim = RainSim
  { rainLayers = return $ RainLayer "dolphin" 3 vel $ M.empty
  , rainColors = attrMap (V.white `on` V.black) $
      return ("dolphin", withStyles [V.italic] $ V.blue `on` V.black)
  , windowSize = Size $ Pos 20 10
  , rainReps = "🐬"
  } where
    vel = Vel $ Pos 1 (-1)
