module Marbles.Eval where
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Arrow
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Monoid
import           Data.Word
import           Reflex.Class

import qualified Reflex.Pure   as P

type Time = Word8


newtype MarbleEvent = MarbleEvent (Map Time Marble) deriving (Show)

eventTimes :: MarbleEvent -> [Time]
eventTimes (MarbleEvent m) = Map.keys m

newtype MarbleBehaviour = MarbleBehaviour (Marble, Map Time Marble) deriving (Show)
newtype Marble = Marble Char deriving (Show)

type TimeM = (->) Time
runTimeM :: TimeM b -> b
runTimeM = ($ 0)

type PureReflexDomain = P.Pure Time

mapToPureBehavior :: MarbleBehaviour -> Behavior PureReflexDomain Marble
mapToPureBehavior (MarbleBehaviour (m0, ms)) = P.Behavior $ \t -> case Map.lookupLE t ms of
  Nothing -> m0
  Just (_, v) -> v

mapToPureEvent :: MarbleEvent -> Event PureReflexDomain Marble
mapToPureEvent (MarbleEvent m) = P.Event $ flip Map.lookup m

evalBehaviour :: [Time] -> Behavior PureReflexDomain a -> (a, Map Time a)
evalBehaviour timeRange (P.Behavior b) =
    (b 0, Map.fromList $ map (id &&& b) timeRange)

evalEvent :: [Time] -> Event PureReflexDomain a -> Map Time a
evalEvent timeRange (P.Event e) =
    Map.mapMaybe id $ Map.fromList $ map (id &&& e) timeRange

marbleHold :: Marble -> MarbleEvent -> (Marble, Map Time Marble)
marbleHold m evs =
    evalBehaviour (eventTimes evs) $ marbleHold' m evs

marbleHold' :: Marble -> MarbleEvent -> (Behavior PureReflexDomain Marble)
marbleHold' v0 e = runTimeM $ hold v0 $ mapToPureEvent e

marbleConstant :: Marble -> Behavior PureReflexDomain Marble
marbleConstant v0 = constant v0

marbleNever :: Event PureReflexDomain Marble
marbleNever = never

marbleOnceE :: MarbleEvent -> Event PureReflexDomain Marble
marbleOnceE = runTimeM . onceE . mapToPureEvent

marbleTag :: MarbleBehaviour -> MarbleEvent -> Event PureReflexDomain Marble
marbleTag b e = tag (mapToPureBehavior b) (mapToPureEvent e)

marbleFmap :: (Marble -> Marble) -> MarbleEvent -> MarbleEvent
marbleFmap f (MarbleEvent e) = MarbleEvent $ fmap f e
