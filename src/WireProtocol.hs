{-# LANGUAGE ScopedTypeVariables #-}

-- Wraps a channel to implement the length-prefixed messages.
-- Does not handle the JSON encoding itself.

module WireProtocol
(
  Channel (),
  openChannel,
  readMessage,
  writeMessage
)
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import System.IO
import System.Environment
import Data.Char (ord)
import Data.Maybe

import Control.Monad.IfElse
import Data.IORef

data Channel = Channel Handle Handle

openChannel :: (Handle, Handle) -> IO Channel
openChannel (hi, ho) = do
  hSetBuffering hi NoBuffering
  return (Channel hi ho)

readMessageLength :: Handle -> IO Int
readMessageLength h = do
  cnum :: IORef Int <- newIORef 0
  cdigit :: IORef Char <- newIORef '0'
  whileM ((readIORef cdigit) >>= (\d -> return (not (d == ':'))) ) ( do
    n <- readIORef cnum
    d <- readIORef cdigit
    writeIORef cnum (n * 10 + ((ord d) - (ord '0')))
    nchar <- hGetChar h
    writeIORef cdigit nchar)
  readIORef cnum

readMessage :: Channel -> IO ByteString
readMessage (Channel hi ho) = do
  length <- readMessageLength hi
  msg <- B.hGet hi length
  whenM (lookupEnv "WIREDEBUG" >>= (\e -> return (isJust e))) (do
    hPutStr stderr "Received Message: "
    B.hPutStr stderr msg
    hPutStrLn stderr ""
    )
  return msg

writeMessage :: Channel -> ByteString -> IO ()
writeMessage (Channel hi ho) bs = do
  let length = B.length bs
  hPutStr ho (show length)
  hPutChar ho ':'
  B.hPutStr ho bs
  hFlush ho
  whenM (lookupEnv "WIREDEBUG" >>= (\e -> return (isJust e))) (do
    hPutStr stderr "Sent Message: "
    B.hPutStr stderr bs
    hPutStrLn stderr ""
    )
