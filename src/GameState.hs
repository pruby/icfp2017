{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameState (
  GameState,
  myId,
  playerCount,
  allSites,
  mines,
  rivers,
  connectionValues,
  claims,
  newGame,
  addClaim,
  findPuntableGraph,
  valueGraphSplit
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import GHC.Generics
import Data.Aeson
import qualified Data.Text
import Data.List (union, intersect)

import Data.Set (Set)
import qualified Data.Set as S

import BaseTypes
import PuntableGraph

data GameExtension = Futures | Splurges | Options deriving (Generic, Read, Show, Ord, Eq);

instance ToJSON GameExtension
instance FromJSON GameExtension

data GameState = GameState {
  myId :: PlayerID,
  playerCount :: Int,
  allSites :: [Site],
  mines :: [Site],
  rivers :: [River],
  connectionValues :: Map Site (M.Map Site Integer),
  claims :: Map (Site, Site) PlayerID,
  options :: Map (Site, Site) PlayerID,
  extensions :: Set GameExtension
} deriving (Generic, Read, Show)

instance ToJSON GameState
instance FromJSON GameState

newGame :: PlayerID -> Int -> [Site] -> [River] -> [GameExtension] -> GameState
newGame myId playerCount mines rivers extns =
  GameState myId playerCount allSites mines rivers connectionValues M.empty M.empty (S.fromList extns)
  where
    connectionValues = M.fromList (zip mines (map mineConnectionValues mines))
    allSites = union (map (snd . riverSites) rivers) (map (fst . riverSites) rivers)
    -- Default zero for every site - stands if we have no connecting rivers
    zeroValues = M.fromList (map (\n -> (n, 0)) allSites)
    -- Left-biased union means that if we have a path, we store its length, zero otherwise
    mineConnectionValues mine = M.union (M.map toInteger (M.fromList (findAllMinimumPathLengths allPathsGraph mine))) zeroValues
    allPathsGraph = buildPuntableGraph allSites (map (\r -> (r, OpenEdge)) rivers)

hasExtension :: GameExtension -> GameState -> Bool
hasExtension extn state = S.member extn (extensions state)

addClaim :: GameState -> (Site, Site) -> PlayerID -> GameState
addClaim state edge player =
  state {claims = updatedClaims}
  where
    updatedClaims = M.insert (orderEdge edge) player (claims state)

findPuntableGraph :: GameState -> PlayerID -> PuntableGraph
findPuntableGraph state player = buildPuntableGraph (allSites state) navigableRivers
  where
    -- Edge is navigable if unclaimed, or claimed by this player
    edgeNavigable e = maybe True ((==) player) (M.lookup e (claims state))
    myRivers = filter (\river -> (M.lookup (riverSites river) (claims state)) == (Just player)) (rivers state)
    openRivers = filter (\river -> (M.notMember (riverSites river) (claims state))) (rivers state)
    navigableRivers = (map (\r -> (r, ClaimedEdge)) myRivers) ++ (map (\r -> (r, OpenEdge)) openRivers)

mineScore :: GameState -> [Site] -> Site -> Integer
mineScore state sites mine = sum (map (\s -> (connectionValue mine s) ^ 2) sites)
  where
    connectionValue mine site = maybe 0 (\ary -> (M.!) ary site) (M.lookup mine (connectionValues state))

scorePuntableGraph :: GameState -> PuntableGraph -> Integer
scorePuntableGraph state puntable = totalScore
  where
    totalScore = sum (map (\mine -> mineScore state (getConnectedSites puntable mine) mine) (mines state))

valueGraphSplit :: GameState -> ([Site], [Site]) -> Integer
valueGraphSplit state (as, bs) = atob + btoa
  where
    amines = as `intersect` (mines state)
    bmines = bs `intersect` (mines state)
    atob = sum (map (mineScore state bs) amines)
    btoa = sum (map (mineScore state as) bmines)
