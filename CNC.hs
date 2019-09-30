{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CNC where

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.Hardware.Serialport
import System.Posix.Unistd
import Data.Bits.Floating
import Data.Int

newtype Number = Number Float deriving (Num, Fractional, Floating)

instance Show Number where
    show (Number n) = show n

instance Binary Number where
    put (Number n) = put (coerceToWord n)
    get = do
        n <- get :: Get Word32
        return $ Number (coerceToFloat n)

type Vector = (Number, Number, Number)

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
    serialPort = "/dev/ttyUSB0",
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
    chuckLength = 40,
    boundMax = (20, 20, 20),
    boundMin = (20, 20, 20)
}

data BitShape = SquareBit | RoundBit
    deriving (Show)

data Direction = Cw | Ccw
    deriving (Show)

instance Binary Direction where
    put (Cw) = putWord8 0
    put (Ccw) = putWord8 1
    get = undefined

data Expr
    = T
    | Val Number
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Sin Expr
    | Cos Expr
    deriving (Show)

instance Num Expr where
    (+) (Val a) (Val b) = Val (a + b)
    (+) a b = Add a b
    (-) (Val a) (Val b) = Val (a - b)
    (-) a b = Sub a b
    (*) (Val a) (Val b) = Val (a * b)
    (*) a b = Mul a b
    -- negate a = undefined
    -- abs a = undefined
    -- signum a = undefined
    fromInteger n = Val (fromInteger n)

instance Fractional Expr where
    (/) (Val a) (Val b) = Val (a / b)
    (/) a b = Div a b
    -- recip :: a -> a
    -- fromRational :: Rational -> a


instance Floating Expr where
    pi = Val pi
    -- exp :: a -> a
    -- log :: a -> a
    -- sqrt :: a -> a
    -- (**) :: a -> a -> a
    -- logBase :: a -> a -> a
    sin (Val a) = Val (sin a)
    sin a = Sin a
    cos (Val a) = Val (cos a)
    cos a = Cos a
    -- tan :: a -> a
    -- asin :: a -> a
    -- acos :: a -> a
    -- atan :: a -> a
    -- sinh :: a -> a
    -- cosh :: a -> a
    -- tanh :: a -> a
    -- asinh :: a -> a
    -- acosh :: a -> a
    -- atanh :: a -> a

instance Binary Expr where
    put n = do
        putWord8 $ numberId n
        case n of
            T -> do
                putNil
            Val a -> do
                put a
            Add a b -> do
                put a
                put b
            Sub a b -> do
                put a
                put b
            Mul a b -> do
                put a
                put b
            Div a b -> do
                put a
                put b
            Sin a -> do
                put a
            Cos a -> do
                put a
    get = do
        c <- getWord8
        case c of
            0 -> return T
            1 -> do
                n <- get
                return (Val n)
            2 -> do
                a <- get
                b <- get
                return (Add a b)

numberId :: Integral a => Expr -> a
numberId (T) = 0
numberId (Val _) = 1
numberId (Add _ _) = 2
numberId (Sub _ _) = 3
numberId (Mul _ _) = 4
numberId (Div _ _) = 5
numberId (Sin _) = 6
numberId (Cos _) = 7

-- 12 * cos (2 * T / pi) + 4 * T - 6

-- exampleHelix = parametric (0, 0, 0) (12 * cos T, 12 * sin T, T / pi, 1) (0, 2 * pi)

data Command
    = Ping
    | IsReady
    | IsMoving
    | GetPosition
    | SetPosition Vector
    | SetFeedRate Number
    | SetSpindleSpeed Number
    | Home
    | Line Vector
    | Parametric (Expr, Expr, Expr, Expr) (Expr, Expr) -- args: equation range
    | Debug
    deriving (Show)

commandId :: Integral a => Command -> a
commandId (Ping) = 0
commandId (IsReady) = 1
commandId (IsMoving) = 2
commandId (GetPosition) = 3
commandId (SetPosition _) = 4
commandId (SetFeedRate _) = 5
commandId (SetSpindleSpeed _) = 6
commandId (Home) = 7
commandId (Line _) = 8
commandId (Parametric _ _) = 9
commandId (Debug) = 10

putNil :: Put
putNil = return ()

instance Binary Command where
    put c = do
        -- putWord8 69
        -- putWord8 69
        putWord8 (commandId c)
        case c of
            Ping -> putNil
            IsReady -> putNil
            IsMoving -> putNil
            Home -> putNil
            GetPosition -> putNil
            SetPosition vec -> put vec
            Line vec -> put vec
            Parametric equ range -> do
                put equ
                put range
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

isReady :: Machine -> IO (Maybe Bool)
isReady machine = do
    s <- sendCommand machine IsReady 1
    return $ case s of
        Just r -> Just $ decode $ LB.fromStrict r
        Nothing -> Nothing

isMoving :: Machine -> IO (Maybe Bool)
isMoving machine = do
    s <- sendCommand machine IsMoving 1
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

line :: Machine -> Vector -> IO Bool
line machine vect = do
    s <- sendCommand machine (Line vect) 0
    return $ s /= Nothing

parametric :: Machine -> Vector -> (Expr, Expr, Expr, Expr) -> (Expr, Expr) -> IO Bool
parametric machine origin equation range = do
    s <- sendCommand machine (Parametric equation range) 0
    return $ s /= Nothing

debug :: Machine -> IO Bool
debug machine = do
    s <- sendCommand machine Debug (-1)
    case s of
        Just m -> B.putStr m >> return True
        Nothing -> return False

-- line :: Machine -> Vector -> IO Bool
-- line machine vect = do
--     s <- sendCommand machine (Line vect) 0
--     return $ s /= Nothing

-- curve :: Machine -> Vector -> Float -> Float -> IO Bool
-- curve machine pos angle eccen = undefined

-- circle :: Machine -> Vector -> Direction -> IO Bool
-- circle machine vect dir = do
--     s <- sendCommand machine (Circle vect dir) 0
--     return $ s /= Nothing

-- helix :: Machine -> Vector -> Direction -> Depth -> Overlap -> IO Bool
-- helix machine vect dir dep ovr = do
--     s <- sendCommand machine (Helix vect dir dep ovr) 0
--     return $ s /= Nothing

-- spiral :: Machine -> Vector -> Direction -> Depth -> Overlap -> IO Bool
-- spiral machine vect dir dep ovr = do
--     s <- sendCommand machine (Spiral vect dir dep ovr) 0
--     return $ s /= Nothing

-- TODO Add goto, which goes to a location via a safe height.

sendCommand :: Machine -> Command -> Int -> IO (Maybe B.ByteString)
sendCommand machine command n = withSerial (serialPort machine) (serialPortSettings machine) $ \ s -> do
    send s $ LB.toStrict $ encodeCommand command

    result <- receive s 3

    -- print result

    let expected = B.pack [69, 69, fromIntegral $ commandId command]

    if result == expected then do
        answer <- receive s (if (n >= 0) then n else 500)

        -- print answer

        let success = (B.length answer == n) || (n == -1)

        return $ if success then (Just answer) else Nothing
    else
        return Nothing

encodeCommand :: Command -> LB.ByteString
encodeCommand command = LB.concat [header, size, bytes]
    where
        header = LB.pack [69, 69]
        size = LB.singleton $ fromIntegral $ LB.length bytes
        bytes = encode command

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
