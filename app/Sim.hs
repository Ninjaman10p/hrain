{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes, Arrows #-}

module Sim
  ( Tick(..)
  , Name
  , mkSim
  , RainSim(..)
  , Pos(..), pX, pY
  , Vel(..), vel
  , Size(..), size
  , withStyle
  , withStyles
  , RainLayer(..)
  ) where

import Control.Monad (void, forever)
import Control.Lens

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow hiding (ArrowPlus (..))

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget, AttrName
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

data Pos = Pos { _pX :: Int, _pY :: Int }
  deriving (Eq, Ord, Show)
makeLenses ''Pos

-- Lenses of Pos
type Dim = forall f. Functor f => (Int -> f Int) -> Pos -> f Pos

newtype Size = Size { _size :: Pos }
makeLenses ''Size

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
  , _windowSize :: Size
  , _rainColors :: AttrMap
  , _interval   :: Int
  }
makeLenses ''RainSim

-- Main
rainSimulator :: App RainSim Tick Name
rainSimulator = App { appDraw = return . renderWindow
                    , appChooseCursor = neverShowCursor
                    , appHandleEvent = handleEvent
                    , appStartEvent = return
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

handleEvent :: RainSim -> BrickEvent Name Tick -> EventM Name (Next RainSim)
handleEvent p (AppEvent (Tick gen)) = continue $
  flip (set rainLayers) p $
    flip evalState gen $ do
      let ticked = tickLayers $ view rainLayers p
      spawned <- spawnRain (view windowSize p) ticked
      return $ trimRain (view windowSize p) spawned
handleEvent p (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt p
handleEvent p _                                     = continue p

tickLayers :: [RainLayer] -> [RainLayer]
tickLayers = map $ \rl -> over rainMap (M.mapKeys (addVel . view rainVel $ rl)) rl

spawnRain :: Size -> [RainLayer] -> State StdGen [RainLayer]
spawnRain s rls = sequence $ do
  rl <- rls
  let spawnLoop = foldr (.) id
                . replicate (view weighting rl)
                $ spawnRainStep s
  return $ runKleisli spawnLoop rl

spawnRainStep :: Size -> Kleisli (State StdGen) RainLayer RainLayer
spawnRainStep s = proc rl -> do
  doSkip <- stateK $ const uniform -< ()
  if doSkip
    then id -< rl
    else do
      pos  <- stateK $ randWindowBorder s . arr (view rainVel)  -< rl
      char <- stateK randChoice           . arr (view rainReps) -< rl
      id -< over rainMap (M.insert pos (Droplet char)) $ rl

stateK :: (a -> s -> (b, s)) -> Kleisli (State s) a b
stateK f = Kleisli $ state . f

windowScale :: Size -> Size
windowScale (Size (Pos x y)) = Size $ Pos (2*x + 2) (2*y + 2)

randWindowBorder :: Size -> Vel -> StdGen -> (Pos, StdGen)
randWindowBorder s v = randChoice . rectPoints v $ windowScale s

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
    lX = trueSize pX wS
    lY = trueSize pY wS
    p (Pos dX dY) _ = 0 <= dX && dX <= lX && 0 <= dY && dY <= lY
  in
    map $ over rainMap (M.filterWithKey p)

trueSize :: Dim -> Size -> Int
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

addWindowBorder :: Size -> Widget Name -> Widget Name
addWindowBorder s w =
  let
    axis d = [0, (2+) . view (size . d) $ s]
    positions = do
      y <- axis pY
      return $ do
        x <- axis pX
        return $ Pos x y
  in
    withAttr "window" . border $
      vBox $ flip map positions $
        hBox . map (\pos -> border $ cropAndFill pos s w)

cropAndFill :: Pos -> Size -> Widget a -> Widget a
cropAndFill p s w = cropToBox p s $ w <+> fill ' '

cropToBox :: Pos -> Size -> Widget a -> Widget a
cropToBox (Pos x y) (Size (Pos dx dy)) =
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
    sqLayer rl = withAttr (view rainStyle rl) . chr . rep <$> (M.lookup p $ view rainMap rl)
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
