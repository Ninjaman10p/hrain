{-# LANGUAGE OverloadedStrings #-}
module Sim
  ( Tick(..)
  , Name
  , mkSim
  , RainSim(..)
  , Pos(..)
  , Vel(..)
  , Size(..)
  , withStyle
  , withStyles
  , RainLayer(..)
  ) where

import Control.Monad (void, forever, (<=<))

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain
  , neverShowCursor
  , continue
  , halt
  , str
  , fill
  , vBox
  , cropLeftBy, cropTopBy
  , cropBottomTo, cropRightTo
  , hBox
  , withAttr
  , AttrName
  , (<+>)
  )
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
import Control.Monad.State.Lazy
  ( state
  , State
  , evalState
  )

-- Types
data Tick = Tick StdGen

type Name = ()

newtype Droplet = Droplet { rep :: Char }
  deriving (Show, Eq)

data Pos = Pos { pX :: Int, pY :: Int }
  deriving (Eq, Ord, Show)

newtype Size = Size { toPos :: Pos }
newtype Vel  = Vel { vToPos :: Pos }
  deriving (Show, Eq)

type Rain = M.Map Pos Droplet

data RainLayer = RainLayer
  { rainStyle :: AttrName
  , rainReps  :: [Char]
  , weighting :: Int
  , rainVel   :: Vel
  , rainMap   :: Rain
  } deriving (Show, Eq)

data RainSim = RainSim
  { rainLayers :: [RainLayer]
  , windowSize :: Size
  , rainColors :: AttrMap
  }

-- Main

app :: App RainSim Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = rainColors
          }

mkSim :: Int -> RainSim -> IO ()
mkSim t rain = do
  chan <- newBChan 10
  _ <- forkIO . forever $ do
    gen <- initStdGen
    writeBChan chan $ Tick gen
    threadDelay t
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app rain

-- Handling events

handleEvent :: RainSim -> BrickEvent Name Tick -> EventM Name (Next RainSim)
handleEvent p (AppEvent (Tick gen)) =
  let
    rainKey = gen
  in
    continue $ p
      { rainLayers = flip evalState rainKey $ do
          let ticked = tickLayers $ rainLayers p
          spawned <- spawnRain (windowSize p) ticked
          return $ trimRain (windowSize p) spawned
      }
handleEvent p (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt p
handleEvent p _                                     = continue p

tickLayers :: [RainLayer] -> [RainLayer]
tickLayers = map $ \rl -> rl { rainMap = M.mapKeys (addVel . rainVel $ rl) $ rainMap rl }

spawnRain :: Size -> [RainLayer] -> State StdGen [RainLayer]
spawnRain s rls = sequence $ do
  rl <- rls
  let spawnLoop = foldK $ replicate (weighting rl) $ \rli -> do
        skip <- state $ uniform
        if skip
        then
          return rli
        else do
          let vel = rainVel rli
              reps = rainReps rli
          pos <- state $ randWindowBorder s vel
          char <- state $ randChoice reps
          return $ rli { rainMap = M.insert pos (Droplet char) $ rainMap rli }
  return $ spawnLoop rl

windowScale :: Size -> Size
windowScale (Size (Pos x y)) = Size $ Pos (2*x + 2) (2*y + 2)

foldK :: Monad m => [a -> m a] -> a -> m a
foldK = foldr (<=<) return

randWindowBorder :: Size -> Vel -> StdGen -> (Pos, StdGen)
randWindowBorder s v = randChoice $ rectPoints v $ windowScale s

rectPoints :: Vel -> Size -> [Pos]
rectPoints (Vel (Pos dx dy)) (Size (Pos sx sy)) = do
  (rX, rY) <-
    [ ([0..sx], [0..dy-1])
    , ([0..sx], [1+sy+dy..sy])
    , ([0..dx-1], [0..sy])
    , ([1+sx+dx..sx], [0..sy])
    ]
  x <- rX
  y <- rY
  return $ Pos x y

randChoice :: [a] -> StdGen -> (a, StdGen)
randChoice l gen =
  let (ind, newGen) = randomR (0, (length l) - 1) gen
  in  (l !! ind, newGen)

trimRain :: Size -> [RainLayer] -> [RainLayer]
trimRain s =
  let
    wS = windowScale s
    lX = 2 + 2*(pX . toPos $ wS)
    lY = 2 + 2*(pY . toPos $ wS)
    predicate (Pos dX dY) _ = 0 <= dX && dX <= lX && 0 <= dY && dY <= lY
  in
    map (\rl -> rl { rainMap = M.filterWithKey predicate $ rainMap rl })

addVel :: Vel -> Pos -> Pos
addVel v (Pos x y) = Pos
  (x + (pX . vToPos $ v))
  (y + (pY . vToPos $ v))

-- Drawing

drawUI :: RainSim -> [Widget Name]
drawUI p = [window p]

window :: RainSim -> Widget Name
window r = C.center
         $ windowBorder (windowSize r)
         $ mkRain r

windowBorder :: Size -> Widget Name -> Widget Name
windowBorder s@(Size (Pos dx dy)) w =
  let
    positions :: [[Pos]]
    positions = do
      y <- [0,1]
      return $ do
        x <- [0,1]
        return $ Pos (x * (dx + 2)) (y * (dy + 2))
  in
    withAttr "window" . border $
      vBox $ flip map positions $
        hBox . map (\pos -> border $ fillCropBox pos s w)

fillCropBox :: Pos -> Size -> Widget a -> Widget a
fillCropBox p s w = cropBox p s $ w <+> fill ' '

cropBox :: Pos -> Size -> Widget a -> Widget a
cropBox (Pos x y) (Size (Pos dx dy)) =
    cropLeftBy x
    . cropTopBy y
    . cropRightTo (x + dx)
    . cropBottomTo (y + dy)

mkRain :: RainSim -> Widget Name
mkRain r = vBox $ do
  y <- [0 .. 2*(pY . toPos . windowSize $ r) + 1]
  return . hBox $ do
    x <- [0 .. 2*(pX . toPos . windowSize $ r) + 1]
    return $ rainSq (Pos x y) r

rainSq :: Pos -> RainSim -> Widget Name
rainSq p r =
  let
    sqLayer rl = withAttr (rainStyle rl) . chr . rep <$> (M.lookup p $ rainMap rl)
  in 
    fromMaybe (chr ' ') . asum . map sqLayer $ rainLayers r

chr :: Char -> Widget a
chr = str . return

-- what are they, lisp programmers forcing me to use brackets?
-- unacceptable
withStyle :: V.Style -> V.Attr -> V.Attr
withStyle = flip V.withStyle

withStyles :: [V.Style] -> V.Attr -> V.Attr
withStyles s l = foldr withStyle l s
