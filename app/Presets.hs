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
  , attrName
  , attrMap
  , AttrMap
  )
import Data.Map as M

rainSim :: RainSim
rainSim = RainSim
  { _rainLayers = do
      (m, w) <- [("rainfg", 10), ("rainbg", 3), ("rainb", 3)]
      return $ RainLayer 
        { _rainStyle = attrName m
        , _rainReps = ",.'"
        , _weighting = w
        , _rainVel = Vel $ Pos (-1) 1
        , _rainMap = M.empty
        }
  , _rainColors = attrMap (V.white `on` V.black)
      [ (attrName "rainfg", withStyles [V.italic] $ fg V.blue)
      , (attrName "rainbg", withStyles [V.dim, V.italic] $ fg V.blue)
      , (attrName "rainb", withStyles [V.italic] $ fg V.white)
      ]
  , _windowSize = Dimensions $ Pos 20 10
  , _interval = 10000
  }

windyRainSim :: RainSim
windyRainSim = RainSim
  { _rainLayers = do
      (m, w) <- [("rainfg", 10), ("rainbg", 3), ("rainb", 3)]
      return $ RainLayer 
        { _rainStyle = attrName m
        , _rainReps = ",.'"
        , _weighting = w
        , _rainVel = Vel $ Pos (-2) 1
        , _rainMap = M.empty
        }
  , _rainColors = attrMap (V.white `on` V.black)
      [ (attrName "rainfg", withStyles [V.italic] $ fg V.blue)
      , (attrName "rainbg", withStyles [V.dim, V.italic] $ fg V.blue)
      , (attrName "rainb", withStyles [V.italic] $ fg V.white)
      ]
  , _windowSize = Dimensions $ Pos 20 10
  , _interval = 10000
  }

stormSim :: RainSim
stormSim = RainSim
  { _rainLayers = rL <> sL
  , _rainColors = attrMap (V.white `on` V.black)
      [ (attrName "rainfg", withStyles [V.italic] $ fg V.blue)
      , (attrName "rainbg", withStyles [V.dim, V.italic] $ fg V.blue)
      , (attrName "rainb", withStyles [V.italic] $ fg V.white)
      , (attrName "lightning", withStyles [] $ fg V.blue)
      ]
  , _windowSize = Dimensions $ Pos 20 10
  , _interval = 10000
  } where
    rL = do
      (m, w) <- [("rainfg", 10), ("rainbg", 3), ("rainb", 3)]
      return $ RainLayer 
        { _rainStyle = attrName m
        , _rainReps = ",.'" 
        , _weighting = w
        , _rainVel = Vel $ Pos (-2) 1
        , _rainMap = M.empty
        }
    sL = return $ RainLayer
        { _rainStyle = attrName "lightning"
        , _rainReps = "🗲" <> replicate 50 ' '
        , _weighting = 1
        , _rainVel = Vel $ Pos 1 3
        , _rainMap = M.empty
        }

snowSim :: RainSim
snowSim = RainSim
  { _rainLayers = do
      (m, w) <- [("snowfg", 10), ("snowbg", 3), ("snowb", 3)]
      return $ RainLayer 
        { _rainStyle = attrName m
        , _rainReps = "*#❄" 
        , _weighting = w
        , _rainVel = Vel $ Pos 1 1
        , _rainMap = M.empty
        }
  , _rainColors = snowColors
  , _windowSize = Dimensions $ Pos 20 10
  , _interval = 100000
  }

snowstormSim :: RainSim
snowstormSim = RainSim
  { _rainLayers = do
      (m, w) <- [("snowfg", 10), ("snowbg", 3), ("snowb", 3)]
      return $ RainLayer 
        { _rainStyle = attrName m
        , _rainReps = "*#❄" 
        , _weighting = w
        , _rainVel = Vel $ Pos (-3) 1
        , _rainMap = M.empty
        }
  , _rainColors = snowColors
  , _windowSize = Dimensions $ Pos 20 10
  , _interval = 10000
  }

matrixSim :: RainSim
matrixSim = RainSim
  { _rainLayers = do
      (m, w) <- [("fg", 10), ("bg", 3)]
      return $ RainLayer 
        { _rainStyle = attrName "matrix" <> attrName m
        , _rainReps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890/:?,.;☺" 
        , _weighting = w
        , _rainVel = Vel $ Pos 0 1
        , _rainMap = M.empty
        }
  , _rainColors = attrMap (V.white `on` V.black)
      [ (attrName "matrix" <> attrName "fg", withStyles [] $ fg V.green)
      , (attrName "matrix" <> attrName "bg", withStyles [V.dim] $ fg V.green)
      ]
  , _windowSize = Dimensions $ Pos 20 10
  , _interval = 100000
  }

rainbowSim :: RainSim
rainbowSim = RainSim
  { _rainLayers = do
      (color, _) <- colors
      (modif, _) <- mods
      return $ RainLayer
        { _rainStyle = attrName modif <> attrName color
        , _weighting = 3
        , _rainVel = Vel $ Pos 1 1
        , _rainReps = "*"
        , _rainMap = M.empty
        }
  , _rainColors = attrMap (V.white `on` V.black) $ do
      (color, vColor) <- colors
      (modif, vMod) <- mods
      return $ (attrName modif <> attrName color, withStyles vMod $ fg vColor)
  , _windowSize = Dimensions $ Pos 20 10
  , _interval = 20000
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
  { _rainLayers = do
      (m, w) <- [("fg", 10), ("bg", 3), ("b", 3)]
      return $ RainLayer 
        { _rainStyle = attrName "rain" <> attrName m
        , _rainReps = "☺" 
        , _weighting = w
        , _rainVel = Vel $ Pos 1 1
        , _rainMap = M.empty
        }
  , _rainColors = attrMap (V.white `on` V.black)
      [ (attrName "rain" <> attrName "fg", withStyles [V.italic] $ V.blue `on` V.black)
      , (attrName "rain" <> attrName "bg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , (attrName "rain" <> attrName "b", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , _windowSize = Dimensions $ Pos 20 10
  , _interval = 50000
  }

catsDogsSim :: RainSim
catsDogsSim = RainSim
  { _rainLayers = do
      (t, r) <-
        [ ([(attrName "fg", 10), (attrName "bg", 3), (attrName "b", 3)], ",.'")
        , ([(attrName "cd", 1)], "🐱🐶")
        ]
      (m, w) <- t
      return $ RainLayer 
        { _rainStyle = attrName "rain" <> m
        , _rainReps = r
        , _weighting = w
        , _rainVel = Vel $ Pos 1 1
        , _rainMap = M.empty
        }
  , _rainColors = attrMap (V.white `on` V.black)
      [ (attrName "rain" <> attrName "fg", withStyles [V.italic] $ V.blue `on` V.black)
      , (attrName "rain" <> attrName "bg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , (attrName "rain" <> attrName "b", withStyles [V.italic] $ V.white `on` V.black)
      ]
  , _windowSize = Dimensions $ Pos 20 10
  , _interval = 20000
  }

knifeSim :: RainSim
knifeSim = RainSim
  { _rainLayers = return $ RainLayer
      { _rainStyle = attrName "knife"
      , _rainReps = "🔪"
      , _weighting = 10
      , _rainVel = Vel $ Pos 1 1
      , _rainMap = M.empty
      }
  , _rainColors = attrMap (V.white `on` V.black) $
      return (attrName "knife", withStyles [V.italic] $ V.blue `on` V.black)
  , _windowSize = Dimensions $ Pos 20 10
  , _interval = 10000
  }

dolphinSim :: RainSim
dolphinSim = RainSim
  { _rainLayers = return $ RainLayer (attrName "dolphin") "🐬" 3 v $ M.empty
  , _rainColors = attrMap (V.white `on` V.black) $
      return (attrName "dolphin", withStyles [V.italic] $ V.blue `on` V.black)
  , _windowSize = Dimensions $ Pos 20 10
  , _interval = 10000
  } where
    v = Vel $ Pos 1 (-1)

snowColors :: AttrMap
snowColors = attrMap (V.white `on` V.black)
      [ (attrName "snowfg", withStyles [V.italic] $ V.blue `on` V.black)
      , (attrName "snowbg", withStyles [V.dim, V.italic] $ V.blue `on` V.black)
      , (attrName "snowb", withStyles [V.italic] $ V.white `on` V.black)
      ]

