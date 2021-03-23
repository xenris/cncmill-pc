{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Protocol where

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

withCNC :: (SerialPort -> IO a) -> IO a
withCNC = withSerial "/dev/ttyACM0" settings
  where
    settings = defaultSerialSettings {commSpeed = CS115200}

data Command
    = Ping -- Say "hi" to mill. (Establish serial connection.)
    | Query
    -- | Calibrate -- Find outer bounds of mill.
    -- | Home -- Move up, then to far corner, using sensors instead of calibrated bounds.
    | Action {dx::Int32, dy::Int32, dz::Int32,
              delayX::Int32,
              delayY::Int32,
              delayZ::Int32}
    -- | GoTo {position::Vec3} -- Move up, move across, then move down to position.
    deriving (Show)

commandId :: Integral a => Command -> a
commandId Ping = 0
commandId Query = 1
commandId Action{} = 2

expectedReplySize :: Command -> Int
expectedReplySize Ping = 0
expectedReplySize Query = 2
expectedReplySize Action{} = 0

putNil :: Put
putNil = return ()

putCommand :: Command -> Put
putCommand c = do
    putWord8 (commandId c)
    case c of
        Ping -> putNil
        Query -> putNil
        -- Calibrate -> putNil
        -- Home -> putNil
        Action{dx, dy, dz, delayX, delayY, delayZ} -> traverse_ putInt32le [dx,dy,dz,delayX,delayY,delayZ]

-- Try 10 times to receive response.
ping :: SerialPort -> IO Bool
ping serialPort = loop 10
    where
        loop 0 = return False
        loop count = do
            response <- sendCommand' serialPort Ping
            case response of
                Just{} -> return True
                Nothing -> loop (count - 1)

sendCommands :: [Command] -> IO ()
sendCommands commands = withCNC $ \serialPort -> do
  ping serialPort
  traverse_ (sendCommand serialPort) commands

sendCommand :: SerialPort -> Command ->  IO (Maybe B.ByteString)
sendCommand serialPort command = do
    case command of
        Action{} -> waitForSpace serialPort
        _ -> pure()
    response <- sendCommand' serialPort command
    case response of
        Just bs -> pure (Just bs)
        Nothing -> error ("Failed to send command: " ++ show command)

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
