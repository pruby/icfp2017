{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}

module GameStrategy
where
  
import BaseTypes
import GameState
import PuntableGraph
import Data.Time.Clock
import System.IO
import System.Environment
import Data.Maybe
import Control.Monad.IfElse
import GHC.Generics
import Control.Monad
import Data.List (intersect, maximumBy)
import System.Random
import Data.IORef

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Tuple
import Control.DeepSeq

data MoveChoice =
    DoPass |
    DoClaim (Int, Int) |
    DoOption (Int, Int) |
    DoSplurge [Int]
    deriving (Generic, Show, Eq)

instance NFData MoveChoice

timeLimit = 0.5

randomFromList :: [a] -> IO (Maybe a)
randomFromList [] = return Nothing
randomFromList ls = do
  rng <- getStdRandom next
  return (Just (ls !! (rng `mod` (length ls))))

randomChoice :: GameState -> IO (MoveChoice, String, Double)
randomChoice state = do
  selection <- randomFromList available
  let choice = maybe DoPass DoClaim selection
  return (choice, "random", 0.0)
  where
    isAvailable m = (not (M.member m (claims state))) && (not (M.member (swap m) (claims state)))
    available = filter isAvailable (map riverSites (rivers state))

selectBridges :: UTCTime -> GameState -> IO (Maybe (MoveChoice, String, Double))
selectBridges startTime state = do
  whenM (lookupEnv "DEBUG" >>= (\e -> return ((isJust e) && (not (null available_bridges))))) (do
    hPutStrLn stderr ("Identified Bridges " ++ (show available_bridges))
    )
  moveBox :: IORef (Maybe (MoveChoice, String, Double)) <- newIORef Nothing
  mapM (\ab -> do
    cmove <- readIORef moveBox
    nmovemaybe <- applyStrategyStep startTime state (tryBridge' ab) cmove
    when (isJust nmovemaybe) (writeIORef moveBox nmovemaybe)
    ) available_bridges
  readIORef moveBox
  where
    isAvailable m = (not (M.member m (claims state))) && (not (M.member (swap m) (claims state)))
    available_bridges = filter isAvailable (findBridges myGraph)
    tryBridge' :: (Site, Site) -> UTCTime -> GameState -> IO (Maybe (MoveChoice, String, Double))
    tryBridge' edge _ _ = do
      let value = fromIntegral (valueGraphSplit state (splitOnBridge myGraph edge))
      whenM (lookupEnv "DEBUG" >>= (\e -> return ((isJust e) && (not (null available_bridges))))) (do
        hPutStrLn stderr ("Bridge " ++ (show edge) ++ " valued at " ++ (show value))
        )
      return (Just (DoClaim edge, "selectBridges", value))
    myGraph = findPuntableGraph state (myId state)
      

randomChokePoint :: UTCTime -> GameState -> IO (Maybe (MoveChoice, String, Double))
randomChokePoint startTime state = do
  rmine1 <- randomFromList (mines state)
  rmine2 <- randomFromList (mines state)
  if (isNothing rmine1 || isNothing rmine2 || rmine1 == rmine2)
     then return Nothing
     else findChokePoint (fromJust rmine1) (fromJust rmine2)
  where
    isAvailable m = (not (M.member m (claims state))) && (not (M.member (swap m) (claims state)))
    findChokePoint mine1 mine2 = do
      let minCut = findMinimumCut myGraph mine1 mine2
      hPutStrLn stderr ("Found cut: " ++ (show minCut))
      let value = (fromIntegral (valueGraphSplit state (splitOnCut myGraph (fromJust minCut)))) / ((fromIntegral (maybe 1 length minCut)) ^ 2)
      let availCut = filter isAvailable $ maybe [] id minCut
      -- TODO use max flow to find a cut
      return (if null availCut then Nothing else (Just (DoClaim (head availCut), "randomChokePoint", value)))
    myGraph = findPuntableGraph state (myId state)

-- Applies a strategy step if and only if we have taken less than timeLimit time so far
-- This will return a modified move if the step returns a move, and it has a better score
-- than the existing option.
applyStrategyStep ::
    UTCTime ->
    GameState ->
    (UTCTime -> GameState -> IO (Maybe (MoveChoice, String, Double))) ->
    Maybe (MoveChoice, String, Double) ->
    IO (Maybe (MoveChoice, String, Double))
applyStrategyStep startTime state step (Just (lastChoice, lastDesc, lastScore)) = do
  ctime <- getCurrentTime
  let elapsed = diffUTCTime ctime startTime
  hPutStrLn stderr ("T+" ++ (show elapsed))
  if (elapsed > timeLimit)
     then return (Just (lastChoice, lastDesc, lastScore))
     else (step startTime state) >>= (\newChoice ->
        case newChoice of
          Nothing -> return (Just (lastChoice, lastDesc, lastScore))
          Just (newMove, newDesc, newScore) ->
            if (newScore > lastScore)
              then return (newChoice `deepseq` newChoice)
              else return (Just (lastChoice, lastDesc, lastScore))
        )
applyStrategyStep startTime state step Nothing = do
  ctime <- getCurrentTime
  let elapsed = diffUTCTime ctime startTime
  hPutStrLn stderr ("T+" ++ (show elapsed))
  if (elapsed > timeLimit)
     then return Nothing
     else step startTime state
  
strategies = [selectBridges] ++ (replicate 16 randomChokePoint)

chooseNextMoveIO :: UTCTime -> GameState -> IO MoveChoice
chooseNextMoveIO startTime state = do
  hPutStrLn stderr "Planning move..."
  t1 <- randomChoice state
  --showTime startTime
  --t2 <- applyStrategyStep startTime state selectBridges (Just t1)
  --let (choiceF, descF, scoreF) = maybe (DoPass, "default", 0.0) id t5
  resultF <- foldM (\tuple strategy -> applyStrategyStep startTime state strategy tuple) (Just t1) strategies
  let (choiceF, descF, scoreF) = maybe (DoPass, "Should not happen", -100.0) id resultF
  hPutStrLn stderr ("Successful strategy was: " ++ (show descF) ++ " with score " ++ (show scoreF))
  ctime <- getCurrentTime
  hPutStrLn stderr ("Planning time was: " ++ (show (diffUTCTime ctime startTime)))
  return choiceF
