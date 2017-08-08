module Main
  where

import System.Environment
import System.IO
import Control.Monad.IfElse
import GameProcess
import Network

connectNetwork :: String -> PortNumber -> IO ()
connectNetwork host port = do
  handle <- connectTo host (PortNumber port)
  hPutStrLn stderr ("Connected to " ++ (show host) ++ ":" ++ (show port))
  interactOnline (handle, handle)

main = do
  args <- getArgs
  whenM (return (length args == 0)) ( do
    hPutStrLn stderr "Running in offline mode"
    interactOffline (stdin, stdout)
    )
  whenM (return (length args == 2)) ( do
    let host = args !! 0
    let port = read (args !! 1)
    hPutStrLn stderr "Running in online mode"
    connectNetwork host port
    )
