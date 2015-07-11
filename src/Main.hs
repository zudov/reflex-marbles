{-# LANGUAGE TypeFamilies, FlexibleContexts, RankNTypes, TupleSections, ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Reflex.Class
import Data.Word
import Control.Monad.Fix
import Data.Map

import Reflex.Test.CrossImpl
import qualified Reflex.Pure as P

main = putStrLn "hello"

type Time = Word8

newtype EventRep = Event [(Time, ConstantRep)] deriving (Show)
newtype BehaviourRep = Behaviour [(Time, ConstantRep)] deriving (Show)
newtype ConstantRep = Constant Int deriving (Show)

type TimeM = (->) Int
type PureReflexDomain = P.Pure Int
type Builder = (t ~ PureReflexDomain, m ~ TimeM) => ((Behavior t a, Event t b) -> m (Behavior t c, Event t d))
type TestCaseConstraint t m = (Reflex t, MonadSample t m, MonadHold t m, MonadFix m, MonadFix (PushM t))

data TestCase = forall a b c d. (Eq c, Eq d, Show c, Show d)
              => TestCase (Map Int a, Map Int b) 
                          (forall m t. TestCaseConstraint t m => (Behavior t a, Event t b)
                                                              -> m (Behavior t c, Event t d))

holdBuilder :: (forall a b c d m t . (TestCaseConstraint t m, Eq c, Eq d, Show c, Show d)
            => (Behavior t a, Event t b)
            -> m (Behavior t c, Event t d))
holdBuilder (_, e) = (,e) <$> hold "123" e
