{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CNC where

import Data.Binary.Put hiding (flush)
import Data.Binary.Get
import Data.Int
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Data.IORef
import Data.Word
import Data.Foldable
-- import Data.Bits.Floating
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.Hardware.Serialport
import System.Posix.Unistd
import Control.Monad.Trans.Writer
import qualified Graphics.Gloss as Gloss
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

-- main = do
--     let portPath = "/dev/ttyACM0"
--     ping portPath
--     sendCommands portPath (runMillM shape1)

main = do
    contents <- readFile "test.ngc"
    let code = filter (\ l -> "G0" `isPrefixOf` l) $ lines contents
    let portPath = "/dev/ttyACM0"
    ping portPath
    sendCommands portPath (runMillM $ traverse_ gCodeToAction code)
    -- traverse_ print (runMillM $ traverse_ gCodeToAction code)

shape1 = do
    setFeedRate 15
    runPen (let l = 80; h = l * sqrt 3 / 2; apo = h/3 in sierpinski (-l/2,-apo) (l/2, -apo) (0,h-apo))

data Mill = Mill {
    serialPort :: String,
    serialPortSettings :: SerialPortSettings,
    bitDiameter :: Float,
    bitLength :: Float,
    bitShape :: BitShape,
    chuckDiameter :: Float,
    chuckLength :: Float
} deriving (Show)

-- defaultSerialPortSettings = SerialPortSettings {
--     commSpeed = CS115200,
--     bitsPerWord = 8,
--     stopb = One,
--     parity = NoParity,
--     flowControl = NoFlowControl,
--     timeout = 1
-- }

mySerialSettings = defaultSerialSettings {commSpeed = CS115200}

myMachine = Mill {
    serialPort = "/dev/ttyACM0",
    serialPortSettings = mySerialSettings,
    bitDiameter = 3.2,
    bitLength = 20,
    bitShape = SquareBit,
    chuckDiameter = 25,
    chuckLength = 40
}

data BitShape = SquareBit | RoundBit
    deriving (Show)

data Direction = Cw | Ccw
    deriving (Show)

type Vec2 = (Double, Double)

type Vec3 = (Double, Double, Double)

data Command
    = Ping -- Say "hi" to mill. (Establish serial connection.)
    | Query
    -- | Calibrate -- Find outer bounds of mill.
    -- | Home -- Move up, then to far corner, using sensors instead of calibrated bounds.
    | Action Int32 Int32 Int32 Int32 Int32 Int32
    -- | GoTo {position::Vec3} -- Move up, move across, then move down to position.
    deriving (Show)

-- toLine :: [Command] -> Gloss.Path
-- toLine [] = []
-- toLine ((GoTo (x,y,z)):xs) = (realToFrac x, realToFrac y) : toLine xs
-- toLine (_:xs) = toLine xs

commandId :: Integral a => Command -> a
commandId (Ping) = 0
commandId (Query) = 1
-- commandId (Calibrate) = 1
commandId (Action {}) = 2
-- commandId (Home) = 2
-- commandId (GoTo {}) = 3

putNil :: Put
putNil = return ()

putCommand c = do
    putWord8 (commandId c)
    case c of
        Ping -> putNil
        Query -> putNil
        -- Calibrate -> putNil
        -- Home -> putNil
        Action a b c d e f -> traverse_ putInt32le [a,b,c] >> traverse_ putInt32le [d,e,f]
        -- GoTo (x,y,z) -> traverse_ putDoublele [x,y,z]

-- New version:

type PortPath = String
type Range = Vec2

-- connect :: PortPath -> IO ()
-- connect port = do
--     s <- openSerial port defaultSerialSettings {commSpeed = CS115200}
--     let d = LB.toStrict $ encodeCommand Ping
--     let ping = sendRecv s d 1 >>= return . (== d)
--     -- takeWhile () (replicate 10 ping)
--     -- send s $ LB.toStrict $ encodeCommand command
--     -- m <- newIORef myMachine
--     return ()

-- home2 :: MillM ()
-- home2 = runCommand Home

-- goto :: Vec3 -> MillM ()
-- goto position = runCommand (GoTo position)

-- path :: Range -> Expr -> Expr -> Expr -> MillM ()
-- path range x y z = undefined

-- Ideal:
-- duck = withMill "/dev/ttyUSB0" $ do
--     goto (40, 50, 0)
--     path (0, 1) T T 0
--     path (0, 1) (cos T) (sin T) 0
--     path (0, 1) 0 0 3
--     goto (0, 0, 0)

-- test = withMill "/dev/ttyUSB0" testShape

circle (cx,cy,z) r = do
    goto (cx + r, cy, z+5)
    goto (cx + r, cy, z)
    for_ [1..1000] $ \i -> do
        let theta = 2 * pi * fromIntegral i / 1000
        goto (cx + r * cos theta, cy + r * sin theta, z)
    goto (cx + r, cy, z+5)

newtype Pen a = Pen { unPen :: StateT (Double,Double,Double) MillM a }
    deriving (Functor, Applicative, Monad)


penUp :: Pen ()
penUp = do (x,y,z) <- Pen (get)
           Pen (put (x,y,z+5))
           Pen (lift (goto (x,y,z+5)))

penDown :: Pen ()
penDown = do (x,y,z) <- Pen (get)
             Pen (put (x,y,z-5))
             Pen (lift (goto (x,y,z-5)))

penGo :: (Double, Double) -> Pen ()
penGo (x, y) = do
    (_,_,z) <- Pen (get)
    Pen (put (x,y,z))
    Pen (lift (goto (x,y,z)))

runPen :: Pen a -> MillM a
runPen (Pen p) = evalStateT p (0,0,0)

mid (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

triangle p1 p2 p3 = do
    penGo p1
    penDown
    penGo p2
    penGo p3
    penGo p1
    penUp

sierpinski :: Vec2 -> Vec2 -> Vec2 -> Pen ()
sierpinski p1 p2 p3 = triangle p1 p2 p3 >> sequence_ (sierpinski' p1 p2 p3)

sierpinski' :: Vec2 -> Vec2 -> Vec2 -> [Pen ()]
sierpinski' p1 p2 p3 = top : rest
    where
        p12 = mid p1 p2
        p23 = mid p2 p3
        p31 = mid p3 p1
        top = do
            penGo p12
            penDown
            penGo p23
            penGo p31
            penGo p12
            penUp
        rest = (zipWith3 (\a b c -> a >> b >> c)
            (sierpinski' p1 p12 p31)
            (sierpinski' p2 p23 p12)
            (sierpinski' p3 p31 p23))

-- sendCommands "/dev/ttyACM0" (runMillM $ setFeedRate 10 >> runPen (let l = 80; h = l * sqrt 3 / 2; apo = h/3 in sierpinski (-l/2,-apo) (l/2, -apo) (0,h-apo)))


goto p = MillM $ tell [Goto p]
setFeedRate fr = MillM $ tell [SetFeedRate fr]

data MillF = Goto (Double,Double,Double) -- mm
           | SetFeedRate Double          -- mm/s

newtype MillM a = MillM {unMillM :: Writer [MillF] a}
    deriving (Functor, Applicative, Monad)

mmPerTick :: Double
mmPerTick = 1.25e-3

runMillM :: MillM () -> [Command]
runMillM (MillM action) = go (0,0,0) 0.5 (execWriter action)
    where go (x1,y1,z1) _ (SetFeedRate fr : steps) = go (x1,y1,z1) fr steps
          go (x1,y1,z1) fr (Goto (x2,y2,z2) : steps)
           | maximum [delayX, delayY, delayZ] == 0 = error (show ((dx^2 + dy^2 + dz^2), distance, time, time'))
           | maximum [delayX, delayY, delayZ] > 0 = line : go (x2',y2',z2') fr steps
           | otherwise = go (x2',y2',z2') fr steps
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
                line = (Action (fromIntegral dx) (fromIntegral dy) (fromIntegral dz)
                                (fromIntegral delayX) (fromIntegral delayY) (fromIntegral delayZ))
          go _ _ [] = []

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

-- testShape = do
--     goto (-100, -100, 0)
--     setFeedRate 5
--     goto (80, 40, 30)
--     -- goto (150, 20, 60)
--     -- goto (0, 0, 0)
--     -- goto (300, 300, 0)
--     circle (250, 300, 0) 100

-- circle (cx, cy, cz) steps = traverse_ (\ i -> goto (x i, y i, 0)) [0 .. steps]
--     where
--         x i = r * cos (2 * pi * i / steps)
--         y i = r * sin (2 * pi * i / steps)
--         r = sqrt (dx * dx + dy * dy)
--         dx = abs (cx - 0) -- FIXME Need current position.
--         dy = abs (cy - 0) -- FIXME Need current position.


-- newtype MillM a = MillM {unMillM :: Writer [Command] a}
--     deriving (Functor, Applicative, Monad)

-- askPort :: MillM SerialPort
-- askPort = MillM ask

-- runCommand :: Command -> MillM ()
-- runCommand c = do port <- askPort
--                   let bytes = runPut (putCommand c)
--                   liftIO (send port bytes)

-- runCommand :: Command -> MillM ()
-- runCommand c = MillM (tell [c])

-- withMill :: PortPath -> MillM () -> IO ()
-- withMill portPath (MillM action) = do
--     let portSettings = defaultSerialSettings {commSpeed = CS115200}
--     withSerial portPath portSettings $ \ port -> do
--         let commands = execWriter action
--             sendCommand c = send port $ LB.toStrict (runPut (putCommand c))
--         mapM_ sendCommand commands

-- ------

-- ping :: Mill -> IO Bool
-- ping mill = ping' mill 10
--     where
--         ping' mill 0 = return False
--         ping' mill count = do
--             s <- sendCommand mill Ping 0
--             case s of
--                 Just b -> return True
                -- Nothing -> ping' mill (count - 1)

-- ping :: PortPath -> IO Bool
-- ping portPath = ping' 10

ping :: PortPath -> IO Bool
ping portPath = withSerial portPath mySerialSettings $ \ s -> ping' s 10
    where
        ping' s 0 = return False
        ping' s count = do
            response <- sendCommand' s Ping
            case response of
                Just _ -> return True
                Nothing -> ping' s (count - 1)

----


-- home :: Mill -> IO Bool
-- home mill = do
--     s <- sendCommand mill Home 0
--     return $ s /= Nothing

-- line :: Mill -> Int16 -> Int16 -> Int16 -> Word16 -> Word16 -> Word16 -> IO Bool
-- line mill x y z sx sy sz = do
--     s <- sendCommand mill (Line x y z sx sy sz) 0
--     return $ s /= Nothing

-- debug :: Mill -> IO Bool
-- debug mill = do
--     s <- sendCommand mill Debug (-1)
--     case s of
--         Just m -> B.putStr m >> return True
--         Nothing -> return False

flushSerial mill = withSerial (serialPort mill) (serialPortSettings mill) $ \ s -> do
    r <- recv s 1000
    flush s
    return ()

sendCommands :: PortPath -> [Command] -> IO ()
sendCommands portPath commands = traverse_ (sendCommand portPath) commands

waitForSpace :: SerialPort -> IO ()
waitForSpace serialPort = do
    Just response <- sendCommand' serialPort Query
    let space = runGet getInt16le (LB.fromStrict response)
    when (space == 0) $ do
        threadDelay 100
        waitForSpace serialPort


sendCommand' :: SerialPort -> Command -> IO (Maybe B.ByteString)
sendCommand' serialPort command = do
    send serialPort $ LB.toStrict $ encodeCommand command
    result <- receive serialPort 3
    let expected = B.pack [69, 69, fromIntegral $ commandId command]
    let n = expectedReplySize command

    if result == expected then do
        answer <- receive serialPort (if (n >= 0) then n else 500)

        let success = (B.length answer == n) || (n == -1)

        return $ if success then (Just answer) else Nothing
    else
        return Nothing

sendCommand :: PortPath -> Command ->  IO (Maybe B.ByteString)
sendCommand portPath command = withSerial portPath mySerialSettings $ \ s -> do
    case command of
        Action{} -> waitForSpace s
        _ -> pure()
    response <- sendCommand' s command
    case response of
        Just bs -> pure (Just bs)
        Nothing -> error ("Failed to send command: " ++ show command)

expectedReplySize Ping = 0
expectedReplySize Query = 2
expectedReplySize Action {} = 0

sendRecv :: SerialPort -> B.ByteString -> Int -> IO B.ByteString
sendRecv serialPort sendBytes recvLength = do
    send serialPort sendBytes

    receive serialPort recvLength

    -- return $ if B.length result == recvLength then Just result else Nothing

    -- let expected = B.pack [69, 69, fromIntegral $ commandId command]

    -- if result == expected then do
    --     answer <- receive s (if (n >= 0) then n else 500)

    --     let success = (B.length answer == n) || (n == -1)

    --     return $ if success then (Just answer) else Nothing
    -- else
    --     return Nothing


encodeCommand :: Command -> LB.ByteString
encodeCommand command = LB.concat [header, bytes]
    where
        header = LB.pack [69, 69]
        bytes = runPut $ putCommand command

-- Like recv, but tries again if it doesn't get enough bytes first try.
-- Useful because sometimes recv doesn't get data of 35 bytes or more consistently.
-- TODO Work out if recv "should" be working ok. Maybe the microcontroller is pausing
--  unexpectedly during the serial transmission?
receive :: SerialPort -> Int -> IO B.ByteString
receive s n = do
    a <- recv s n

    if (B.length a) == n then
        return a
    else do
        usleep 100000
        b <- recv s (n - (B.length a))
        return (B.append a b)
