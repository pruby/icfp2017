{-# LANGUAGE FlexibleInstances #-}

module BaseTypes
(
  Site,
  PlayerID,
  River (),
  makeRiver,
  riverSites,
  orderEdge
)
  where

import Data.Aeson
import Data.Vector
import Data.Scientific

type Site = Int
type PlayerID = Int

data River = River {
  riverSites :: (Site, Site)
} deriving (Read, Show)

instance FromJSON River where
  parseJSON v = withArray "River" (\a -> do
    pair <- parseJSON v
    let (source, target) = pair
    return (makeRiver (source, target))
    ) v
    
instance ToJSON River where
  toJSON (River (source, target)) = Array (Data.Vector.fromList [Number sourceS, Number targetS])
    where
        sourceS = scientific (toInteger source) 0
        targetS = scientific (toInteger target) 0

orderEdge :: (Ord a) => (a, a) -> (a, a)
orderEdge (a, b) = if b > a then (a, b) else (b, a)

makeRiver :: (Site, Site) -> River
makeRiver pair = River (orderEdge pair)


