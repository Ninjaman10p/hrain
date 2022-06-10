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
  , stormSim
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
  { rainLayers = do
      (m, w) <- [("rainfg", 10), ("rainbg", 3), ("rainb", 3)]
      return $ RainLayer 
        { rainStyle = m
        , rainReps = ",.'"
        , weighting = w
        , rainVel = Vel $ Pos (-1) 1
        , rainMap = M.empty
        }
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ fg V.blue)
      , ("rainbg", withStyles [V.dim, V.italic] $ fg V.blue)
      , ("rainb", withStyles [V.italic] $ fg V.white)
      ]
  , windowSize = Size $ Pos 20 10
  , interval = 10000
  }

windyRainSim :: RainSim
windyRainSim = RainSim
  { rainLayers = do
      (m, w) <- [("rainfg", 10), ("rainbg", 3), ("rainb", 3)]
      return $ RainLayer 
        { rainStyle = m
        , rainReps = ",.'"
        , weighting = w
        , rainVel = Vel $ Pos (-2) 1
        , rainMap = M.empty
        }
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ fg V.blue)
      , ("rainbg", withStyles [V.dim, V.italic] $ fg V.blue)
      , ("rainb", withStyles [V.italic] $ fg V.white)
      ]
  , windowSize = Size $ Pos 20 10
  , interval = 10000
  }

stormSim :: RainSim
stormSim = RainSim
  { rainLayers = rL <> sL
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rainfg", withStyles [V.italic] $ fg V.blue)
      , ("rainbg", withStyles [V.dim, V.italic] $ fg V.blue)
      , ("rainb", withStyles [V.italic] $ fg V.white)
      , ("lightning", withStyles [] $ fg V.blue)
      ]
  , windowSize = Size $ Pos 20 10
  , interval = 10000
  } where
    rL = do
      (m, w) <- [("rainfg", 10), ("rainbg", 3), ("rainb", 3)]
      return $ RainLayer 
        { rainStyle = m
        , rainReps = ",.'" 
        , weighting = w
        , rainVel = Vel $ Pos (-2) 1
        , rainMap = M.empty
        }
    sL = return $ RainLayer
        { rainStyle = "lightning"
        , rainReps = "🗲" <> replicate 50 ' '
        , weighting = 1
        , rainVel = Vel $ Pos 1 3
        , rainMap = M.empty
        }

snowSim :: RainSim
snowSim = RainSim
  { rainLayers = do
      (m, w) <- [("snowfg", 10), ("snowbg", 3), ("snowb", 3)]
      return $ RainLayer 
        { rainStyle = m
        , rainReps = "*#❄" 
        , weighting = w
        , rainVel = Vel $ Pos 1 1
        , rainMap = M.empty
        }
  , rainColors = attrMap (V.white `on` V.black)
      [ ("snowfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("snowbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("snowb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , interval = 100000
  }

snowstormSim :: RainSim
snowstormSim = RainSim
  { rainLayers = do
      (m, w) <- [("snowfg", 10), ("snowbg", 3), ("snowb", 3)]
      return $ RainLayer 
        { rainStyle = m
        , rainReps = "*#❄" 
        , weighting = w
        , rainVel = Vel $ Pos (-3) 1
        , rainMap = M.empty
        }
  , rainColors = attrMap (V.white `on` V.black)
      [ ("snowfg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("snowbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("snowb", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , interval = 10000
  }

matrixSim :: RainSim
matrixSim = RainSim
  { rainLayers = do
      (m, w) <- [("fg", 10), ("bg", 3)]
      return $ RainLayer 
        { rainStyle = "matrix" <> m
        , rainReps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890/:?,.;☺" 
        , weighting = w
        , rainVel = Vel $ Pos 0 1
        , rainMap = M.empty
        }
  , rainColors = attrMap (V.white `on` V.black)
      [ ("matrix" <> "fg", withStyles [] $ fg V.green)
      , ("matrix" <> "bg", withStyles [V.dim] $ fg V.green)
      ]
  , windowSize = Size $ Pos 20 10
  , interval = 100000
  }

rainbowSim :: RainSim
rainbowSim = RainSim
  { rainLayers = do
      (color, _) <- colors
      (modif, _) <- mods
      return $ RainLayer
        { rainStyle = modif <> color
        , weighting = 3
        , rainVel = Vel $ Pos 1 1
        , rainReps = "*"
        , rainMap = M.empty
        }
  , rainColors = attrMap (V.white `on` V.black) $ do
      (color, vColor) <- colors
      (modif, vMod) <- mods
      return $ (modif <> color, withStyles vMod $ fg vColor)
  , windowSize = Size $ Pos 20 10
  , interval = 20000
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
  { rainLayers = do
      (m, w) <- [("fg", 10), ("bg", 3), ("b", 3)]
      return $ RainLayer 
        { rainStyle = "rain" <> m
        , rainReps = "☺" 
        , weighting = w
        , rainVel = Vel $ Pos 1 1
        , rainMap = M.empty
        }
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rain" <> "fg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rain" <> "bg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rain" <> "b", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , interval = 50000
  }

catsDogsSim :: RainSim
catsDogsSim = RainSim
  { rainLayers = do
      (t, r) <-
        [ ([("fg", 10), ("bg", 3), ("b", 3)], ",.'")
        , ([("cd", 1)], "🐱🐶")
        ]
      (m, w) <- t
      return $ RainLayer 
        { rainStyle = "rain" <> m
        , rainReps = r
        , weighting = w
        , rainVel = Vel $ Pos 1 1
        , rainMap = M.empty
        }
  , rainColors = attrMap (V.white `on` V.black)
      [ ("rain" <> "fg", withStyles [V.italic] $ V.blue `on` V.black)
      , ("rain" <> "bg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , ("rain" <> "b", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , windowSize = Size $ Pos 20 10
  , interval = 20000
  }

knifeSim :: RainSim
knifeSim = RainSim
  { rainLayers = return $ RainLayer
      { rainStyle = "knife"
      , rainReps = "🔪"
      , weighting = 10
      , rainVel = Vel $ Pos 1 1
      , rainMap = M.empty
      }
  , rainColors = attrMap (V.white `on` V.black) $
      return ("knife", withStyles [V.italic] $ V.blue `on` V.black)
  , windowSize = Size $ Pos 20 10
  , interval = 10000
  }

dolphinSim :: RainSim
dolphinSim = RainSim
  { rainLayers = return $ RainLayer "dolphin" "🐬" 3 vel $ M.empty
  , rainColors = attrMap (V.white `on` V.black) $
      return ("dolphin", withStyles [V.italic] $ V.blue `on` V.black)
  , windowSize = Size $ Pos 20 10
  , interval = 10000
  } where
    vel = Vel $ Pos 1 (-1)
