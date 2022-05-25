{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void, forever, (<=<))
import System.Environment (getArgs)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain
  , neverShowCursor
  , continue
  , halt
  , str
  , fill
  , fg
  , attrMap, on
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

type Rain = M.Map Pos Droplet

data RainLayer = RainLayer
  { rainStyle :: AttrName
  , weighting :: Int
  , rainMap   :: Rain
  } deriving (Show, Eq)

data RainSim = RainSim
  { rainLayers :: [RainLayer]
  , windowSize :: Size
  , rainReps   :: [Char]
  , rainVel    :: Vel
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    []            -> mkSim 10000 rainSim
    ("snow":_)    -> mkSim 100000 snowSim
    ("rainbow":_) -> mkSim 20000 rainbowSim
    ("matrix":_)  -> mkSim 100000 matrixSim
    _             -> mkSim 10000 rainSim

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
  -- , rainReps = "☺"    -- It's raining dwarves
  -- , rainReps = ",.'🐱🐶" -- It's raining cat's and dogs
  -- , rainReps = "🔪" -- It's raining knives
  , rainVel = Vel $ Pos (-1) 1
  }

snowSim :: RainSim
snowSim = RainSim
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
  , rainReps = "*#❄"
  , rainVel = Vel $ Pos 1 1
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

-- Handling events

handleEvent :: RainSim -> BrickEvent Name Tick -> EventM Name (Next RainSim)
handleEvent p (AppEvent (Tick gen)) =
  let
    rainKey = gen
  in
    continue $ p
      { rainLayers = flip evalState rainKey $ do
          let vel = rainVel p
              ticked = tickLayers vel $ rainLayers p
          spawned <- spawnRain (rainReps p) (windowSize p) (rainVel p) ticked
          return $ trimRain (windowSize p) spawned
      }
handleEvent p (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt p
handleEvent p _                                     = continue p

tickLayers :: Vel -> [RainLayer] -> [RainLayer]
tickLayers v = map $ \rl -> rl { rainMap = M.mapKeys (addVel v) $ rainMap rl }

spawnRain :: [Char] -> Size -> Vel -> [RainLayer] -> State StdGen [RainLayer]
spawnRain reps s _ rls = sequence $ do
  rl <- rls
  let spawnLoop = foldK $ replicate (weighting rl) $ \rli -> do
        pos <- state $ randWindowBorder s
        char <- state $ randChoice reps
        return $ rli { rainMap = M.insert pos (Droplet char) $ rainMap rli }
  return $ spawnLoop rl

windowScale :: Size -> Size
windowScale (Size (Pos x y)) = Size $ Pos (2*x + 2) (2*y + 2)

foldK :: Monad m => [a -> m a] -> a -> m a
foldK = foldr (<=<) return

randWindowBorder :: Size -> StdGen -> (Pos, StdGen)
randWindowBorder s = randChoice $ rectPoints $ windowScale s

rectPoints :: Size -> [Pos]
rectPoints (Size (Pos sx sy)) =
  [Pos x y | x <- [0..sx], y <- [0,sy]]
  ++
  [Pos x y | x <- [0,sx], y <- [1..sy-1]]

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

doN :: Int -> (a -> a) -> a -> a
doN n = foldr (.) id . take n . repeat

compose :: [a -> a] -> a -> a
compose = foldr (.) id
