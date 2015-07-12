{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Reflex.Dom
import Data.Map (Map)
import Data.Word
import Control.Monad
import Data.List
import Text.Read
import qualified Data.Map as Map

import Marbles.Eval

main :: IO ()
main = mainWidget $ do
  divClass "container" $ do
    divClass "row" $ do
      divClass "col-md-12" $ do
        holdDemo
  return ()

functionHeader name firstParam = text (name ++ " :: ") >> firstParam
functionArgument content = do
  divClass "argument" (text "-> " >> content)

holdDemo = do
  divClass "demo" $ do
    inMarble <- functionHeader "hold" marbleInput
    inEvent <- functionArgument $ renderMarbleEvent $ constDyn $ Map.fromList [(5, Marble 'a'), (11, Marble 'b')]
    outEvent <- $(qDyn [| evalBehaviour [1..20] $ marbleHold' $(unqDyn [| inMarble |])
                                                              (MarbleEvent $(unqDyn [| inEvent |]))
                                                              |])
    el "br" $ pure ()
    text "->"
    renderMarbleBehaviour =<< mapDyn snd outEvent

marbleInput :: (MonadWidget t m, Reflex t) => m (Dynamic t Marble)
marbleInput = mapDyn (Marble . maybe '⊥' fst . uncons) =<< (_textInput_value <$> textInput def)
   
renderMarbleEvent :: (MonadWidget t m)
                 => (Dynamic t (Map Time Marble))
                 -> m (Dynamic t (Map Time Marble))
renderMarbleEvent marblesDyn = do
  el "table" $ do
    el "thead" $ el "tr" $ do
      marblesTimesDyn <- mapDyn Map.keys marblesDyn
      simpleList marblesTimesDyn $ \t ->
          el "td" $ display t
    el "tbody" $ el "tr" $ do
      marblesValsDyn <- mapDyn Map.elems marblesDyn
      simpleList marblesValsDyn $ \t ->
          el "td" . dynText =<< mapDyn (\(Marble x) -> [x]) t
  return marblesDyn

renderMarbleBehaviour :: (MonadWidget t m)
                      => (Dynamic t (Map Time Marble))
                      -> m (Dynamic t (Map Time Marble))
renderMarbleBehaviour = renderMarbleEvent
