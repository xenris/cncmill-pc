module CNC where

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.Hardware.Serialport
import System.Posix.Unistd
import Data.Bits.Floating
import Data.Int

data Vector = Vector Float Float Float
    deriving (Show)

instance Binary Vector where
    put (Vector x y z) = do
        putFloat x
        putFloat y
        putFloat z
    get = do
        x <- getFloat
        y <- getFloat
        z <- getFloat
        return $ Vector x y z

putFloat n = put (floor (n * 100000) :: Int32)

getFloat = do
    n <- get :: Get Int32
    return $ (fromIntegral n) / 100000

data Machine = Machine {
    serialPort :: String,
    serialPortSettings :: SerialPortSettings,
    bitDiameter :: Float,
    bitLength :: Float,
    bitShape :: BitShape,
    chuckDiameter :: Float,
    chuckLength :: Float,
    boundMax :: Vector,
    boundMin :: Vector
}

myMachine = Machine {
    serialPort = "/dev/ttyACM0",
    serialPortSettings = SerialPortSettings {
        commSpeed = CS38400,
        bitsPerWord = 8,
        stopb = One,
        parity = NoParity,
        flowControl = NoFlowControl,
        timeout = 1
    },
    bitDiameter = 3.2,
    bitLength = 20,
    bitShape = SquareBit,
    chuckDiameter = 25,
    chuckLength = 40,
    boundMax = Vector 20 20 20,
    boundMin = Vector 20 20 20
}

data BitShape = SquareBit | RoundBit
    deriving (Show)

data Direction = Cw | Ccw
    deriving (Show)

instance Binary Direction where
    put (Cw) = putWord8 0
    put (Ccw) = putWord8 1
    get = undefined

type Depth = Float
type Overlap = Float

data Command
    = Ping
    | Ready
    | GetPosition
    | SetPosition Vector
    | MoveTo Vector
    | Line Vector
    | Circle Vector Direction
    | Helix Vector Direction Depth Overlap
    deriving (Show)

commandId :: Integral a => Command -> a
commandId (Ping) = 0
commandId (Ready) = 1
commandId (GetPosition) = 2
commandId (SetPosition _) = 3
commandId (MoveTo _) = 4
commandId (Line _) = 5
commandId (Circle _ _) = 6
commandId (Helix _ _ _ _) = 7

putNil = putList ([] :: [Word8])

instance Binary Command where
    put c = do
        putWord8 69
        putWord8 69
        putWord8 (commandId c)
        case c of
            Ping -> putNil
            Ready -> putNil
            GetPosition -> putNil
            SetPosition vec -> put vec
            MoveTo vec -> put vec
            Line vec -> put vec
            Circle vec dir -> do
                put vec
                put dir
            Helix vec dir dep ovr -> do
                put vec
                put dir
                putFloat dep
                putFloat ovr
    get = undefined

ping :: Machine -> IO Bool
ping machine = ping' machine 10
    where
        ping' machine 0 = return False
        ping' machine count = do
            s <- sendCommand machine Ping 0
            case s of
                Just b -> return True
                Nothing -> ping' machine (count - 1)

ready :: Machine -> IO (Maybe Bool)
ready machine = do
    s <- sendCommand machine Ready 1
    return $ case s of
        Just r -> Just $ decode $ LB.fromStrict r
        Nothing -> Nothing

getPosition :: Machine -> IO (Maybe Vector)
getPosition machine = do
    s <- sendCommand machine GetPosition (3 * 4)
    return $ case s of
        Just b -> Just $ decode $ LB.fromStrict b
        Nothing -> Nothing

flushSerial machine = withSerial (serialPort machine) (serialPortSettings machine) $ \ s -> do
    r <- recv s 1000
    flush s
    return ()

setPosition :: Machine -> Vector -> IO Bool
setPosition machine vect = do
    s <- sendCommand machine (SetPosition vect) 0
    return $ s /= Nothing

moveTo :: Machine -> Vector -> IO Bool
moveTo machine vect = do
    s <- sendCommand machine (MoveTo vect) 0
    return $ s /= Nothing

line :: Machine -> Vector -> IO Bool
line machine vect = do
    s <- sendCommand machine (Line vect) 0
    return $ s /= Nothing

circle :: Machine -> Vector -> Direction -> IO Bool
circle machine vect dir = do
    s <- sendCommand machine (Circle vect dir) 0
    return $ s /= Nothing

helix :: Machine -> Vector -> Direction -> Depth -> Overlap -> IO Bool
helix machine vect dir dep ovr = do
    s <- sendCommand machine (Helix vect dir dep ovr) 0
    return $ s /= Nothing

sendCommand :: Machine -> Command -> Int -> IO (Maybe B.ByteString)
sendCommand machine command n = withSerial (serialPort machine) (serialPortSettings machine) $ \ s -> do
    send s $ LB.toStrict $ encode command

    result <- receive s 3

    let expected = B.pack [69, 69, fromIntegral $ commandId command]

    if result == expected then do
        answer <- receive s n

        return $ if (B.length answer == n) then (Just answer) else Nothing
    else
        return Nothing

-- Like recv, but tries again if it doesn't get enough bytes first try.
-- Useful because sometimes recv doesn't get data of 35 bytes or more consistently.
-- TODO Work out if recv "should" be working ok. Maybe the microcontroller is pausing
--  unexpectedly during the serial transmission.
receive :: SerialPort -> Int -> IO B.ByteString
receive s n = do
    a <- recv s n

    if (B.length a) == n then
        return a
    else do
        usleep 100000
        b <- recv s (n - (B.length a))
        return (B.append a b)
