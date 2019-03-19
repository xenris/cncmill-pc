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
        put (floor (x * 1000) :: Int32)
        put (floor (y * 1000) :: Int32)
        put (floor (z * 1000) :: Int32)
    get = do
        x <- get :: Get Int32
        y <- get :: Get Int32
        z <- get :: Get Int32
        return $ Vector ((fromIntegral x) / 1000) ((fromIntegral y) / 1000) ((fromIntegral z) / 1000)

data Machine = Machine {
    serialPort :: String,
    bitDiameter :: Float,
    bitLength :: Float,
    bitShape :: BitShape,
    chuckDiameter :: Float,
    chuckLength :: Float,
    boundMax :: Vector,
    boundMin :: Vector
} deriving (Show)

myMachine = Machine {
    serialPort = "/dev/ttyUSB0",
    bitDiameter = 3.2,
    bitLength = 20,
    bitShape = SquareBit,
    chuckDiameter = 25,
    chuckLength = 40,
    boundMax = Vector 20 20 20,
    boundMin = Vector 20 20 20
}

mySerialPortSettings = SerialPortSettings {
    commSpeed = CS38400,
    bitsPerWord = 8,
    stopb = One,
    parity = NoParity,
    flowControl = NoFlowControl,
    timeout = 1
}

data BitShape = SquareBit | RoundBit
    deriving (Show)

data Direction = Cw | Ccw
    deriving (Show)

data Command
    = Ping
    | GetPosition
    | SetPosition Vector
    | MoveTo Vector
    deriving (Show)

instance Binary Command where
    put (Ping) = do
        put (69 :: Word8)
        put (69 :: Word8)
        put (0 :: Word8)
    put (GetPosition) = do
        put (69 :: Word8)
        put (69 :: Word8)
        put (1 :: Word8)
    put (SetPosition vec) = do
        put (69 :: Word8)
        put (69 :: Word8)
        put (2 :: Word8)
        put vec
    put (MoveTo vec) = do
        put (69 :: Word8)
        put (69 :: Word8)
        put (3 :: Word8)
        put vec
    get = undefined

ping :: Machine -> IO Bool
ping machine = ping' machine 10
    where
        ping' machine 0 = return False
        ping' machine count = withSerial (serialPort machine) mySerialPortSettings $ \ s -> do
            send s $ LB.toStrict $ encode Ping
            r <- receive s 3
            if (r == B.pack [69, 69, 0]) then
                return True
            else
                ping' machine (count - 1)

getPosition :: Machine -> IO (Maybe Vector)
getPosition machine = withSerial (serialPort machine) mySerialPortSettings $ \ s -> do
    send s $ LB.toStrict $ encode GetPosition
    header <- receive s 3
    if (header == B.pack [69, 69, 1]) then do
        vect <- receive s (3 * 4)
        return $ Just $ decode $ LB.fromStrict vect
    else
        return Nothing


flushSerial machine = withSerial (serialPort machine) mySerialPortSettings $ \ s -> do
    r <- recv s 1000
    return ()

setPosition :: Machine -> Vector -> IO Bool
setPosition machine position = withSerial (serialPort machine) mySerialPortSettings $ \ s -> do
    send s $ LB.toStrict $ encode $ SetPosition position
    header <- receive s 3
    return (header == B.pack [69, 69, 2])

-- move :: Vector -> IO Bool
-- move = undefined

moveTo :: Machine -> Vector -> IO Bool
moveTo machine position = withSerial (serialPort machine) mySerialPortSettings $ \ s -> do
    send s $ LB.toStrict $ encode $ MoveTo position
    header <- receive s 3
    return (header == B.pack [69, 69, 3])

line :: Machine -> Vector -> IO Bool
line = undefined

circle :: Machine -> Vector -> Direction -> IO Bool
circle = undefined

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
        b <- recv s n
        return (B.append a b)
