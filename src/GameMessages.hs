{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameMessages
where

import BaseTypes

import Data.Aeson
import Data.Text
import Data.Aeson.Types (Parser, parse)

import GHC.Generics
import Data.Maybe
import GameState (GameState, newGame)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Data.HashMap.Lazy as H

import Control.Applicative

data SiteMessage = SiteMessage {
  id :: Site
  } deriving (Generic, Show, Read)

instance FromJSON SiteMessage
instance ToJSON SiteMessage

data RiverMessage = RiverMessage {
  source :: Site,
  target :: Site
  } deriving (Generic, Show, Read)

instance FromJSON RiverMessage
instance ToJSON RiverMessage

data MapMessage = MapMessage {
  sites :: [SiteMessage],
  rivers :: [RiverMessage],
  mines :: [Site]
  } deriving (Generic, Show, Read)

instance FromJSON MapMessage

data GameInitMessage = GameInitMessage {
  punter :: PlayerID,
  punters :: Int,
  map :: MapMessage
  } deriving (Generic, Show, Read)

instance FromJSON GameInitMessage

data Score = Score PlayerID Int
  deriving (Show, Read)

instance FromJSON Score where
  parseJSON = withObject "Score" (\o -> do
    pid <- o .: "punter"
    score <- o .: "score"
    return (Score pid score)
    )

data GameMove =
  PassMove PlayerID |
  ClaimMove PlayerID Site Site |
  SplurgeMove PlayerID [Site] |
  OptionMove PlayerID Site Site
  deriving (Show, Read) 
    
instance FromJSON GameMove where
  parseJSON = withObject "GameMove" (\o -> (parseClaimMove o) <|> (parsePassMove o) <|> (parseOptionMove o) <|> (parseSplurgeMove o))
    where
      parseClaimMove o = do
        claim :: Object <- o .: "claim"
        player :: PlayerID <- claim .: "punter"
        source :: Site <- claim .: "source"
        target :: Site <- claim .: "target"
        return (ClaimMove player source target)
      parseOptionMove o = do
        option :: Object <- o .: "option"
        player :: PlayerID <- option .: "punter"
        source :: Site <- option .: "source"
        target :: Site <- option .: "target"
        return (OptionMove player source target)
      parseSplurgeMove o = do
        splurge :: Object <- o .: "splurge"
        player :: PlayerID <- splurge .: "punter"
        route :: [Site] <- splurge .: "route"
        return (SplurgeMove player route)
      parsePassMove o = do
        pass :: Object <- o .: "pass"
        player :: PlayerID <- pass .: "punter"
        return (PassMove player)

data RoundMessage = MidRoundMessage {
    moves :: [GameMove]
  } | FinalRoundMessage {
    moves :: [GameMove],
    scores :: [Score]
  }deriving (Generic, Show, Read)

instance FromJSON RoundMessage where
  parseJSON = withObject "RoundMessage" (\o -> do
    let isFinal = H.member "stop" o
    (if isFinal then parseFinalRoundMessage else parseMidRoundMessage) o
    )
    where
      parseMidRoundMessage o = do
        n :: Object <- o .: "move"
        moves :: [GameMove] <- n .: "moves"
        return (MidRoundMessage moves)
      parseFinalRoundMessage o = do
        n :: Object <- o .: "stop"
        moves :: [GameMove] <- n .: "moves"
        scores :: [Score] <- n .: "scores"
        return (FinalRoundMessage moves scores)

data HandshakeResponse = HandshakeResponse {
    you :: Text
  } deriving (Generic, Show, Read)

instance FromJSON HandshakeResponse

parseInitialMessage :: Value -> Parser GameState
parseInitialMessage im = do
  initMessage :: GameInitMessage <- parseJSON im
  let concreteRivers = Prelude.map constructRiver (rivers (GameMessages.map initMessage))
  return (newGame 
      (punter initMessage)
      (punters initMessage)
      (mines (GameMessages.map initMessage))
      concreteRivers
      []
      )
  where
    constructRiver :: RiverMessage -> River
    constructRiver rm = makeRiver (source rm, target rm)

setupGameState :: ByteString -> GameState
setupGameState im =
  if isJust decoded
    then
      case (parse parseInitialMessage (fromJust decoded)) of
          (Success state) -> state
          (Error a) -> error a
    else
      error "Invalid JSON Input"
  where
    decoded :: Maybe Value
    decoded = decode im

offlineMessageParser :: Value -> Parser (GameState, Bool, Maybe RoundMessage)
offlineMessageParser o =
  (parseInitialMessage o >>= (\s -> return (s, True, Nothing))) <|> (do
    roundMessage :: RoundMessage <- parseJSON o
    withObject "GameState" (\jo -> do
      state :: GameState <- jo .: "state"
      return (state, False, Just roundMessage)
      ) o
    )
