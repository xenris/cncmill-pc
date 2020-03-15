module CNC where

import Data.Binary
import Data.Int
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.Hardware.Serialport
import System.Posix.Unistd

data Machine = Machine {
    serialPort :: String,
    serialPortSettings :: SerialPortSettings,
    bitDiameter :: Float,
    bitLength :: Float,
    bitShape :: BitShape,
    chuckDiameter :: Float,
    chuckLength :: Float
}

myMachine = Machine {
    serialPort = "/dev/ttyACM0",
    serialPortSettings = SerialPortSettings {
        commSpeed = CS115200,
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
    chuckLength = 40
}

data BitShape = SquareBit | RoundBit
    deriving (Show)

data Direction = Cw | Ccw
    deriving (Show)

data Command
    = Ping
    | Home
    | Line Int16 Int16 Int16 Word16 Word16 Word16
    | Debug
    deriving (Show)

commandId :: Integral a => Command -> a
commandId (Ping) = 0
commandId (Home) = 1
commandId (Line _ _ _ _ _ _) = 2
commandId (Debug) = 3

putNil :: Put
putNil = return ()

instance Binary Command where
    put c = do
        putWord8 (commandId c)
        case c of
            Ping -> putNil
            Home -> putNil
            Line x y z sx sy sz -> do
                put x
                put y
                put z
                put sx
                put sy
                put sz
            Debug -> putNil
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

home :: Machine -> IO Bool
home machine = do
    s <- sendCommand machine Home 0
    return $ s /= Nothing

line :: Machine -> Int16 -> Int16 -> Int16 -> Word16 -> Word16 -> Word16 -> IO Bool
line machine x y z sx sy sz = do
    s <- sendCommand machine (Line x y z sx sy sz) 0
    return $ s /= Nothing

debug :: Machine -> IO Bool
debug machine = do
    s <- sendCommand machine Debug (-1)
    case s of
        Just m -> B.putStr m >> return True
        Nothing -> return False

flushSerial machine = withSerial (serialPort machine) (serialPortSettings machine) $ \ s -> do
    r <- recv s 1000
    flush s
    return ()

sendCommand :: Machine -> Command -> Int -> IO (Maybe B.ByteString)
sendCommand machine command n = withSerial (serialPort machine) (serialPortSettings machine) $ \ s -> do
    send s $ LB.toStrict $ encodeCommand command

    result <- receive s 3

    let expected = B.pack [69, 69, fromIntegral $ commandId command]

    if result == expected then do
        answer <- receive s (if (n >= 0) then n else 500)

        let success = (B.length answer == n) || (n == -1)

        return $ if success then (Just answer) else Nothing
    else
        return Nothing

encodeCommand :: Command -> LB.ByteString
encodeCommand command = LB.concat [header, bytes]
    where
        header = LB.pack [69, 69]
        bytes = encode command

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
