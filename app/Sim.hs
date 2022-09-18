{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes, FlexibleContexts #-}

module Sim
  ( Tick(..)
  , Name
  , mkSim
  , RainSim(..)
  , Pos(..), pX, pY
  , Vel(..), vel
  , Dimensions(..), size
  , withStyle
  , withStyles
  , RainLayer(..)
  ) where

import Control.Monad
import Control.Lens

import Brick
import System.Random
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border
-- import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.State.Class
import Control.Monad.State.Lazy (evalState)

-- Types
newtype Tick = Tick StdGen

type Name = ()

newtype Droplet = Droplet { rep :: Char }
  deriving (Show, Eq)

data Pos = Pos { _pX :: Int, _pY :: Int }
  deriving (Eq, Ord, Show)
makeLenses ''Pos

-- Lenses of Pos
type Dim = forall f. Functor f => (Int -> f Int) -> Pos -> f Pos

newtype Dimensions = Dimensions { _size :: Pos }
makeLenses ''Dimensions

newtype Vel  = Vel { _vel :: Pos }
  deriving (Show, Eq)
makeLenses ''Vel

type Rain = M.Map Pos Droplet

data RainLayer = RainLayer
  { _rainStyle :: AttrName
  , _rainReps  :: [Char]
  , _weighting :: Int
  , _rainVel   :: Vel
  , _rainMap   :: Rain
  } deriving (Show, Eq)
makeLenses ''RainLayer

data RainSim = RainSim
  { _rainLayers :: [RainLayer]
  , _windowSize :: Dimensions
  , _rainColors :: AttrMap
  , _interval   :: Int
  }
makeLenses ''RainSim

-- Main
rainSimulator :: App RainSim Tick Name
rainSimulator = App { appDraw = return . renderWindow
                    , appChooseCursor = neverShowCursor
                    , appHandleEvent = handleEvent
                    , appStartEvent = return ()
                    , appAttrMap = view rainColors
                    }

mkSim :: RainSim -> IO ()
mkSim rain = do
  chan <- newBChan 10
  _ <- forkIO . forever $ do
    gen <- initStdGen
    writeBChan chan $ Tick gen
    threadDelay $ view interval rain
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) rainSimulator rain

-- Handling events

handleEvent :: BrickEvent Name Tick -> EventM Name RainSim ()
handleEvent (AppEvent (Tick gen)) = do
  winSize <- view windowSize <$> get
  modify $ over rainLayers $
    trimRain winSize
    . flip evalState gen . spawnRain winSize
    . tickLayers
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt
handleEvent _ = return ()

tickLayers :: [RainLayer] -> [RainLayer]
tickLayers = map $ \rl -> over rainMap (M.mapKeys (addVel . view rainVel $ rl)) rl

spawnRain :: MonadState StdGen m => Dimensions -> [RainLayer] -> m [RainLayer]
spawnRain dim layers = sequence $ do
  layer <- layers
  let loops = view weighting layer
      spawnLoop = foldr (<=<) return $
        replicate loops $ spawnRainStep dim
  return $ spawnLoop layer

spawnRainStep :: MonadState StdGen m => Dimensions -> RainLayer -> m RainLayer
spawnRainStep s layer = do
  skip <- state uniform
  if skip
    then return layer
    else do
      pos <- state $ randWindowBorder s $ view rainVel layer
      char <- state $ randChoice $ view rainReps layer
      return $ over rainMap (M.insert pos (Droplet char)) layer

windowScale :: Dimensions -> Dimensions
windowScale (Dimensions (Pos x y)) = Dimensions $ Pos (2*x + 2) (2*y + 2)

randWindowBorder :: Dimensions -> Vel -> StdGen -> (Pos, StdGen)
randWindowBorder s v = randChoice . rectPoints v $ windowScale s

rectPoints :: Vel -> Dimensions -> [Pos]
rectPoints (Vel (Pos dx dy)) (Dimensions (Pos sx sy)) = do
  (rX, rY) <-
    [ ([0..sx], [0..dy-1])
    , ([0..sx], [1+sy+dy..sy])
    , ([0..dx-1], [0..sy])
    , ([1+sx+dx..sx], [0..sy])
    ]
  Pos <$> rX <*> rY

randChoice :: [a] -> StdGen -> (a, StdGen)
randChoice l gen =
  let (ind, newGen) = randomR (0, length l - 1) gen
  in  (l !! ind, newGen)

trimRain :: Dimensions -> [RainLayer] -> [RainLayer]
trimRain s = map $ over rainMap (M.filterWithKey p)
  where wS = windowScale s
        lX = trueSize pX wS
        lY = trueSize pY wS
        p (Pos dX dY) _ = 0 <= dX && dX <= lX && 0 <= dY && dY <= lY

trueSize :: Dim -> Dimensions -> Int
trueSize d = (+2) . (*2) . view (size . d)

addVel :: Vel -> Pos -> Pos
addVel v = addDim pX v . addDim pY v

addDim :: Dim -> Vel -> Pos -> Pos
addDim d v = over d (+ view (vel . d) v)

-- Drawing

renderWindow :: RainSim -> Widget Name
renderWindow r = C.center
         $ addWindowBorder (view windowSize r)
         $ renderScene r

addWindowBorder :: Dimensions -> Widget Name -> Widget Name
addWindowBorder s w =
  let
    axis d = [0, (2+) . view (size . d) $ s]
    positions = do
      y <- axis pY
      return $ do
        x <- axis pX
        return $ Pos x y
  in
    withAttr (attrName "window") . border $
      vBox $ flip map positions $
        hBox . map (\pos -> border $ cropAndFill pos s w)

cropAndFill :: Pos -> Dimensions -> Widget a -> Widget a
cropAndFill p s w = cropToBox p s $ w <+> fill ' '

cropToBox :: Pos -> Dimensions -> Widget a -> Widget a
cropToBox (Pos x y) (Dimensions (Pos dx dy)) =
   cropLeftBy x
   . cropTopBy y
   . cropRightTo (x + dx)
   . cropBottomTo (y + dy)

renderScene :: RainSim -> Widget Name
renderScene r = vBox $ do
  let axis d = [0 .. (1+) . (2*) $ view (windowSize . size . d) r]
  y <- axis pY
  return . hBox $ do
    x <- axis pX
    return $ renderTile (Pos x y) r

renderTile :: Pos -> RainSim -> Widget Name
renderTile p r =
  let
    sqLayer rl = withAttr (view rainStyle rl) . chr . rep <$> M.lookup p (view rainMap rl)
  in 
    fromMaybe (chr ' ') . asum . map sqLayer $ view rainLayers r

chr :: Char -> Widget a
chr = str . return

-- what are they, lisp programmers forcing me to use brackets?
-- unacceptable
withStyle :: V.Style -> V.Attr -> V.Attr
withStyle = flip V.withStyle

withStyles :: [V.Style] -> V.Attr -> V.Attr
withStyles = flip $ foldr withStyle
