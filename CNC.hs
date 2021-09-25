{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CNC where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.State
import           Control.Monad.Trans.Writer
import           Data.Binary.Get
import           Data.Binary.Put hiding (flush)
-- import Data.Bits.Floating
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Foldable
import           Data.IORef
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Word
import qualified Graphics.Gloss as Gloss
import           System.Hardware.Serialport
import           System.Posix.Unistd

import           Protocol

type Vec2 = (Double, Double)
type Vec3 = (Double, Double, Double)

-- import qualified Graphics.UI.GLUT as GLUT
-- import qualified Graphics.Rendering.OpenGL as GL

-- main = Gloss.animate windowSettings Gloss.black animation
--     where
--         windowSettings = Gloss.InWindow "Nice Window" (round windowWidth, round windowHeight) (10, 10)
--         -- picture = color white $ Circle 80
--         -- picture = color white (Line $ (startX, startY) : testlines)
--         -- picture = Gloss.color Gloss.white (Gloss.Line $ (startX, startY) : toLine (execWriter $ unMillM testShape))
--         animation t = Gloss.color Gloss.white (Gloss.Line $ take (1 + floor (3 * t)) $ (startX, startY) : toLine (execWriter $ unMillM testShape))
--         testlines = [(0, 0), (50, 50), (70, 10)]
--         startX = negate $ windowWidth / 2 - 50
--         startY = negate $ windowHeight / 2 - 50
--         windowWidth = 1000
--         windowHeight = 800

main :: IO ()
main = do
  -- Just ping the device.
  withCNC $ \serialPort -> do
    ping serialPort >>= print

-- main = do
--     contents <- readFile "test.ngc"
--     let code = filter (\ l -> "G0" `isPrefixOf` l) $ lines contents
--     sendCommands (runMillM $ traverse_ gCodeToAction code)
--     -- traverse_ print (runMillM $ traverse_ gCodeToAction code)

runGCode :: String -> IO ()
runGCode gcode = sendCommands $ runMillM $ traverse_ parseGCodeLine (lines gcode)

parseGCodeLine :: String -> MillM ()
parseGCodeLine s = runCommand $ words $ cleanLine s

runCommand :: [String] -> MillM ()
runCommand xs = do
    (sx, sy, sz) <- getCurrentPosition

    let mg = find ((== 'G') . head) xs
        mx = find ((== 'X') . head) xs
        my = find ((== 'Y') . head) xs
        mz = find ((== 'Z') . head) xs
        mf = find ((== 'F') . head) xs
        mi = find ((== 'I') . head) xs
        mj = find ((== 'J') . head) xs
        x = maybe sx (read . tail) mx
        y = maybe sy (read . tail) my
        z = maybe sz (read . tail) mz
        i = maybe 0 (read . tail) mi
        j = maybe 0 (read . tail) mj
        cx = sx + i
        cy = sy + j
        angle = atan2 (-j) (-i) - atan2 (y) (x)

    maybe (return ()) (setFeedRate . (/ 60) . read . tail) mf

    case mg of
        Just "G00" -> goto (x, y, z)
        Just "G01" -> goto (x, y, z)
        Just "G02" -> circleG (x, y) (i, j) True
        Just "G03" -> circleG (x, y) (i, j) False
        _ -> return ()

cleanLine :: String -> String
cleanLine [] = []
cleanLine ('%':xs) = cleanLine xs
cleanLine ('(':xs) = cleanLine $ skipComment xs 1
cleanLine (')':xs) = error "Unexpected ')' found"
cleanLine (x:xs) = x : cleanLine xs

skipComment :: String -> Int -> String
skipComment xs 0 = xs
skipComment [] n = error "Expected ')'"
skipComment ('(':xs) n = skipComment xs (n + 1)
skipComment (')':xs) n = skipComment xs (n - 1)
skipComment (_:xs) n = skipComment xs n

shape1 :: MillM ()
shape1 = do
    setFeedRate 15
    runPen Down (let l = 80; h = l * sqrt 3 / 2; apo = h/3 in
                    sierpinski (-l/2,-apo) (l/2, -apo) (0,h-apo) 4)

-- circle :: Vec2 -> Double -> Double -> MillM ()
-- circle (cx, cy) angle depth = do
--     (x, y, z) <- getCurrentPosition
--     let r = sqrt ((cx - x) * (cx - x) + (cy - y) * (cy - y))
--         startAngle = atan2 (y - cy) (x - cx)
--         steps = round (2 * r * (abs angle + 1)) :: Int
--     for_ [1..steps] $ \ i -> do
--         let t = startAngle - angle * fromIntegral i / fromIntegral steps
--             d = depth * fromIntegral i / fromIntegral steps
--         goto (cx + r * cos t, cy + r * sin t, z + d)

circleG :: Vec2 -> Vec2 -> Bool -> MillM ()
circleG (x, y) (i, j) clockwise = do
    (sx, sy, sz) <- getCurrentPosition
    let r = sqrt (i * i + j * j)
        startAngle = atan2 (sy - cy) (sx - cx)
        steps = ceiling (r * (abs angle) + 1) :: Int
        cx = sx + i
        cy = sy + j
        endAngle = atan2 (y - cy) (x - cx)
        angle' = endAngle - startAngle
        angle = angle' + (if angle' > pi then -2 * pi else if angle' < (-pi) then 2 * pi else 0)
    for_ [1..steps] $ \ i -> do
        let t = startAngle + angle * fromIntegral i / fromIntegral steps
        goto (cx + r * cos t, cy + r * sin t, sz)

data PenState = Up | Down
  deriving Eq

newtype Pen a = Pen { unPen :: StateT PenState MillM a }
    deriving (Functor, Applicative, Monad)

penUp :: Pen ()
penUp = Pen $ do
  ps <- get
  when (ps == Down) $ do
    (x,y,z) <- lift getCurrentPosition
    put Up
    lift $ goto (x,y,z+5)

penDown :: Pen ()
penDown = Pen $ do
  ps <- get
  when (ps == Up) $ do
    (x,y,z) <- lift getCurrentPosition
    put Down
    lift $ goto (x,y,z-5)

penCurrent :: Pen Vec2
penCurrent = Pen $ do
  (x,y,z) <- lift getCurrentPosition
  return (x,y)

penGo :: Vec2 -> Pen ()
penGo (x, y) = Pen $ do
    (_,_,z) <- lift getCurrentPosition
    lift $ goto (x,y,z)

runPen :: PenState -> Pen a -> MillM a
runPen initialState (Pen m) = evalStateT m initialState

mid :: Vec2 -> Vec2 -> Vec2
mid (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

circle :: Vec2 -> Double -> Pen ()
circle (cx,cy) r = do
  penUp
  penGo (cx + r, cy)
  penDown
  for_ [1..100] $ \i -> do
    let theta = 2 * pi * fromIntegral i / 100
    penGo (cx + r * cos theta,
           cy + r * sin theta)
  penUp

triangle :: Vec2 -> Vec2 -> Vec2 -> Pen ()
triangle p1 p2 p3 = do
    penUp
    penGo p1
    penDown
    penGo p2
    penGo p3
    penGo p1

-- Do n levels then return to the starting position.
sierpinski :: Vec2 -> Vec2 -> Vec2 -> Int -> Pen ()
sierpinski p1 p2 p3 n = do
  pos <- penCurrent
  triangle p1 p2 p3
  sequence_ (take n $ sierpinski' p1 p2 p3)
  penUp
  penGo pos

-- Each element of the list is a level of the fractal
sierpinski' :: Vec2 -> Vec2 -> Vec2 -> [Pen ()]
sierpinski' p1 p2 p3 = top : rest
    where
        p12 = mid p1 p2
        p23 = mid p2 p3
        p31 = mid p3 p1
        top = triangle p12 p23 p31
        rest = (zipWith3 (\a b c -> a >> b >> c)
                (sierpinski' p1 p12 p31)
                (sierpinski' p2 p23 p12)
                (sierpinski' p3 p31 p23))

sierp =
  let l = 80;
      h = l * sqrt 3 / 2;
      apo = h/3
  in sierpinski (-l/2,-apo) (l/2, -apo) (0,h-apo) 3

-- sendCommands (runMillM $ setFeedRate 10 >> runPen (let l = 80; h = l * sqrt 3 / 2; apo = h/3 in sierpinski (-l/2,-apo) (l/2, -apo) (0,h-apo)))

fromTo :: Double -> Double -> Double -> [Double]
fromTo a b step
  | a < b = takeWhile (<b) [a + fromIntegral i * step | i <- [0..]] ++ [b]
  | a > b = takeWhile (>b) [a - fromIntegral i * step | i <- [0..]] ++ [b]

runGasket :: IO ()
runGasket = sendCommands (runMillM gasket)

gasket :: MillM ()
gasket = do
  setFeedRate 4.0
  let bitSize = 3.2/2
  goto (10/2 + bitSize, 0, 0)
  goto (10/2 + bitSize, 0, -9.0)
  let circle z r =
        for_ [1..100] $ \i -> do
        let theta = 2 * pi * fromIntegral i / 100
        goto (r * cos theta, r * sin theta, z)
  for_ (fromTo (10.0/2) (8.0/2) 0.3) $ \r -> do
    circle (-9.0) (r + bitSize)
  goto (8.0/2 + bitSize, 0, -5.8)
  for_ (fromTo (8.0/2) (6.0/2) 0.3) $ \r -> do
    circle (-5.8) (r + bitSize)
  goto (8.0/2 + bitSize, 0, -5.8)
  goto (8.0/2 + bitSize, 0, 0)


-- Basic operations.
-- Being a functor allows these to have a return value
-- All units in mm, mm/s
data MillF a = Goto Vec3 a
             | SetFeedRate Double a
             | GetCurrentPosition (Vec3 -> a) -- return value is Vec3
             deriving Functor

newtype MillM a = MillM {unMillM :: Free MillF a}
    deriving (Functor, Applicative, Monad)

goto :: Vec3 -> MillM ()
goto p = MillM $ liftF (Goto p ())

setFeedRate :: Double -> MillM ()
setFeedRate fr = MillM $ liftF (SetFeedRate fr ())

getCurrentPosition :: MillM Vec3
getCurrentPosition = MillM $ liftF (GetCurrentPosition id)

mmPerTick :: Double
mmPerTick = 1.25e-3

runMove :: Vec3 -> Double -> Vec3 -> ([Command], Vec3)
runMove (x1,y1,z1) fr (x2,y2,z2)
  -- Skip null action
  | maximum [abs dx, abs dy, abs dz] == 0 = ([], (x1,y1,z1))
  -- Move in a line
  | maximum [delayX, delayY, delayZ] > 0 = ([line], (x2',y2',z2'))
  -- otherwise throw an error if all delays are 0
  | otherwise = error (show ((dx^2 + dy^2 + dz^2), distance, time, time'))
  where
      dx = round $ (x2 - x1) / mmPerTick :: Int64
      dy = round $ (y2 - y1) / mmPerTick :: Int64
      dz = round $ (z2 - z1) / mmPerTick :: Int64
      x2' = x1 + fromIntegral dx * mmPerTick
      y2' = y1 + fromIntegral dy * mmPerTick
      z2' = z1 + fromIntegral dz * mmPerTick
      distance = sqrt (fromIntegral (dx^2 + dy^2 + dz^2)) * mmPerTick
      time = distance / fr -- seconds
      time' = round (time * 16e6) -- clock cycles
      delayX = if dx /= 0 then time' `div` abs dx else 0
      delayY = if dy /= 0 then time' `div` abs dy else 0
      delayZ = if dz /= 0 then time' `div` abs dz else 0
      line = Action {
        dx = (fromIntegral dx),
        dy = (fromIntegral dy),
        dz = (fromIntegral dz),
        delayX = (fromIntegral delayX),
        delayY = (fromIntegral delayY),
        delayZ = (fromIntegral delayZ)}

runMillM :: MillM () -> [Command]
runMillM (MillM action) = go (0,0,0) 0.5 action
    where go _ _ (Pure a) = []
          go pos _ (Free (SetFeedRate newFr k)) = go pos newFr k
          go pos fr (Free (GetCurrentPosition k)) = go pos fr (k pos)
          go pos fr (Free (Goto pos2 k)) = case runMove pos fr pos2 of
                                             (actions, pos2') -> actions ++ go pos2' fr k

gCodeToAction :: String -> MillM ()
gCodeToAction line = do
    setFeedRate (f * 5 / 1000)
    goto (x, y, z)
        where
            Just x = lookup 'X' dict
            Just y = lookup 'Z' dict
            Just z = lookup 'Y' dict
            f = fromMaybe 1000 (lookup 'F' dict)
            words' = words line
            letters = map head words'
            values = map read (map tail words') :: [Double]
            dict = zip letters values
