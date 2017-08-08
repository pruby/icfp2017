{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameProcess
where
  
import System.IO
import System.Environment
import System.Exit
import WireProtocol
import Data.Aeson
import Data.Aeson.Types (Parser, parse)
import Control.Monad.IfElse
import Control.Monad
import Data.Text (Text)
import Data.IORef
import Data.Time.Clock
import Data.Maybe
import qualified Data.HashMap.Lazy as H

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import BaseTypes
import GameMessages
import GameState
import GameStrategy

myName :: Text
myName = "Lambda In Closure Act"

validHandshakeResponse :: Maybe HandshakeResponse -> Bool
validHandshakeResponse Nothing = False
validHandshakeResponse (Just (HandshakeResponse pname)) = (pname == myName)

handshake :: Channel -> IO ()
handshake channel = do
  let msg = Data.Aeson.encode (object [ "me" .= myName ])
  writeMessage channel msg
  response <- readMessage channel
  let json = Data.Aeson.decode(response)
  whenM
    (return (not (validHandshakeResponse json)))
    (fail "Bad handshake")

onlineGameInit :: Channel -> IO (IORef GameState)
onlineGameInit channel = do
  gameInitMessage <- readMessage channel
  let initialState = setupGameState gameInitMessage
  whenM (lookupEnv "DEBUG" >>= (\e -> return (isJust e))) (do
    hPutStr stderr "Loaded Initial State: "
    hPutStrLn stderr (show initialState)
    hFlush stderr
    )
  gameStateReady channel False initialState

gameStateReady :: Channel -> Bool -> GameState -> IO (IORef GameState)
gameStateReady channel isOffline initialState = do
  s <- newIORef initialState
  let ready = if isOffline
      then object [ ("ready" .= (myId initialState)), ("state" .= initialState) ]
      else object [ "ready" .= (myId initialState) ]
  writeMessage channel (Data.Aeson.encode ready)
  return s

applyOtherPlayerMoves :: RoundMessage -> GameState -> GameState
applyOtherPlayerMoves rm state = Prelude.foldl applyOneMove state (moves rm)

applyOneMove :: GameState -> GameMove -> GameState
applyOneMove st (ClaimMove pid source dest) = addClaim st (source, dest) pid 
applyOneMove st (SplurgeMove pid route) = foldl (\st (source, dest) -> addClaim st (source, dest) pid) st (zip route (tail route))
applyOneMove st _ = st

moveChoiceToGameMove :: MoveChoice -> GameState -> GameMove
moveChoiceToGameMove (DoClaim (src, tgt)) state = ClaimMove (myId state) src tgt
moveChoiceToGameMove (DoPass) state = PassMove (myId state)
moveChoiceToGameMove (DoOption (src, tgt)) state = OptionMove (myId state) src tgt
moveChoiceToGameMove (DoSplurge route) state = SplurgeMove (myId state) route

chosenMoveToMessage :: MoveChoice -> GameState -> Bool -> Value
chosenMoveToMessage (DoClaim (src, tgt)) state isOffline = object [
  "state" .= maybeState,
  "claim" .= (object [
    "punter" .= (myId state),
    "source" .= src,
    "target" .= tgt 
    ])
  ]
  where
    maybeState = if isOffline then Just state else Nothing
chosenMoveToMessage (DoOption (src, tgt)) state isOffline = object [
  "state" .= maybeState,
  "option" .= (object [
    "punter" .= (myId state),
    "source" .= src,
    "target" .= tgt 
    ])
  ]
  where
    maybeState = if isOffline then Just state else Nothing
chosenMoveToMessage (DoSplurge route) state isOffline = object [
  "state" .= maybeState,
  "option" .= (object [
    "punter" .= (myId state),
    "route" .= route
    ])
  ]
  where
    maybeState = if isOffline then Just state else Nothing
chosenMoveToMessage (DoPass) state isOffline = object [
  "state" .= maybeState,
  "pass" .= (object [
    "punter" .= (myId state)
    ])
  ]
  where
    maybeState = if isOffline then Just state else Nothing

printScores endState (FinalRoundMessage _ scores) = do
  hPutStrLn stderr "Game over!"
  hPutStrLn stderr ("I was player " ++ (show (myId endState)))
  hPutStrLn stderr ("The scores were " ++ (show scores))
  return ()
printScores _ _ = return ()

interactOneOnlineTurn :: Channel -> IORef GameState -> IO ()
interactOneOnlineTurn channel stateBox = do
  msg <- readMessage channel
  startTime <- getCurrentTime
  let roundMessage :: RoundMessage = fromJust (decode msg)
  modifyIORef stateBox (applyOtherPlayerMoves roundMessage)
  case roundMessage of
    (FinalRoundMessage _ scores) -> (do
      endState <- readIORef stateBox
      printScores endState roundMessage
      exitWith ExitSuccess)
    (MidRoundMessage _) -> (do
      state <- readIORef stateBox
      newState <- executeAction channel startTime state False
      writeIORef stateBox newState
      )

executeAction :: Channel -> UTCTime -> GameState -> Bool -> IO GameState
executeAction channel startTime state isOffline = do
  chosenMove <- chooseNextMoveIO startTime state
  hPutStrLn stderr (show chosenMove)
  let message = chosenMoveToMessage chosenMove state isOffline
  writeMessage channel (encode message)
  return (applyOneMove state (moveChoiceToGameMove chosenMove state))

interactOnline :: (Handle, Handle) -> IO ()
interactOnline (input, output) = do
  channel <- openChannel (input, output)
  handshake channel
  stateBox <- onlineGameInit channel
  forever (interactOneOnlineTurn channel stateBox)

interactOffline :: (Handle, Handle) -> IO ()
interactOffline (input, output) = do
  channel <- openChannel (input, output)
  handshake channel
  msg <- readMessage channel
  startTime <- getCurrentTime
  let obj :: Maybe Value = decode msg
  when (isJust obj) (do
    let parsed = parse offlineMessageParser (fromJust obj)
    case parsed of
        Success (state, isInitialRound, roundMsg) -> (do
          if isInitialRound
            then void (gameStateReady channel True state)
            else case roundMsg of
              (Just finalMessage@(FinalRoundMessage _ _)) -> (do
                  printScores state finalMessage
                  exitWith ExitSuccess)
              (Just msg@(MidRoundMessage _)) -> (
                  let nextState = applyOtherPlayerMoves msg state in void (executeAction channel startTime nextState True)
                  )
              _                                -> (error "Can't play round")
          )
        Error m -> error m
    )
