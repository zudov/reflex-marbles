{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

import Reflex.Dom
import Data.Map (Map)
import Data.Monoid
import Data.Foldable
import Data.Bifunctor
import Data.Word
import Data.Function
import Control.Monad
import Data.List
import Text.Read
import qualified Data.Map as Map

import Control.Monad.IO.Class

import Marbles.Eval
import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Event as Dom
import qualified GHCJS.DOM.EventM as Dom
import qualified GHCJS.DOM.UIEvent as Dom
import qualified GHCJS.DOM.Element as Dom
import qualified GHCJS.DOM.Document as Dom

mouseLocal :: (Dom.IsElement e, Dom.IsUIEvent uiE) => e -> uiE -> IO (Int, Int)
mouseLocal e event = do
  x <- Dom.uiEventGetLayerX event
  y <- Dom.uiEventGetLayerY event

  ex <- Dom.elementGetOffsetLeft e
  ey <- Dom.elementGetOffsetTop e
  return (x - round ex, y - round ey)

mouseMove_ :: (Dom.IsElement e, MonadWidget t m) => e -> m (Event t (Int, Int))
mouseMove_ e = wrapDomEvent e Dom.elementOnmousemove (liftIO . mouseLocal e =<< Dom.event) 

mouseDown_ :: (Dom.IsElement e, MonadWidget t m) => e -> m (Event t (Int, Int))
mouseDown_ e = wrapDomEvent e Dom.elementOnmousedown (liftIO . mouseLocal e =<< Dom.event) 

mouseUp_ :: (Dom.IsElement e, MonadWidget t m) => e -> m (Event t (Int, Int))
mouseUp_ e = wrapDomEvent e Dom.elementOnmouseup (liftIO . mouseLocal e =<< Dom.event) 

isMouseDown :: (Dom.IsElement e, MonadWidget t m) => e -> m (Behavior t Bool)
isMouseDown e = do
  mouseDown <- mouseDown_ e
  mouseUp <- mouseUp_ e
  hold False $ leftmost [const False <$> mouseUp, const True <$> mouseDown]

draggableDiv :: MonadWidget t m
             => String -- ^ class
             -> Dynamic t Int
             -> m (Dynamic t Int)
draggableDiv = undefined
  
main :: IO ()
main = mainWidget $ do
  divClass "container" $ do
    holdDemo
  return ()

functionHeader name firstParam = do
  elClass "form" "form-inline functionHeader" $ do
    elClass "p" "form-control-static" $ text (name ++ " :: ")
    firstParam

functionArgument content = do
  divClass "argument" $ do 
    elClass "span" "glyphicon glyphicon-arrow-right" $ pure ()
    content

holdDemo = do
  divClass "demo panel panel-default" $ do
    rec
      inMarble <- divClass "row" $ do
        divClass "col-md-1 functionName" $ text "hold ::"
        divClass "col-md-11" $ marbleInput
      (parent, inEvent) <- elAttr' "div" ("class" =: "row") $ do
        divClass "col-md-1 functionName" $ text "→"
        divClass "col-md-11" $ inputMarbleEvent parent $ Map.fromList [(20, Marble 'a'), (60, Marble 'b')]
      divClass "row" $ do
        divClass "col-md-1 functionName" $ text "→"
        divClass "col-md-11" $ do
          outEvent <- $(qDyn [| evalBehaviour [1..100] $ marbleHold' $(unqDyn [| inMarble |])
                                                                  (MarbleEvent $(unqDyn [| inEvent |]))
                                                                  |])
          renderMarbleBehaviour =<< mapDyn snd outEvent
          return outEvent
    return ()

marbleInput :: (MonadWidget t m, Reflex t) => m (Dynamic t Marble)
marbleInput = divClass "marbleInput" $ do
  mapDyn (Marble . maybe '⊥' fst . uncons) =<< (_textInput_value <$> textInput config)
  where config = def { _textInputConfig_initialValue = "⊥"
                     , _textInputConfig_attributes = constDyn
                                (  "maxlength" =: "1"
                                <> "size" =: "1"
                                <> "class" =: "form-control")
                     }

unMarble (Marble x) = x

inputMarbleEvent :: (MonadWidget t m)
                 => El t
                 -> Map Time Marble
                 -> m (Dynamic t (Map Time Marble))
inputMarbleEvent parent event0 = do
  divClass "marbleEvent" $ do
    divClass "timeline" $ pure ()
    r <- forM (Map.toList event0) $ \(t, Marble c) -> do
      rec mouseDown <- mouseDown_ $ _el_element slider
          mouseUp <- mouseUp_ $ _el_element parent
          isDown <- hold False $ leftmost [const False <$> mouseUp, const True <$> mouseDown]
          evMove <- mouseMove_ $ _el_element parent
          dynLeft <- mapDyn fst =<< (holdDyn (t * 10,0) $ gate isDown evMove)
          sliderAttrs <- forDyn dynLeft $ \left -> ("class" =: "marble" <> "style" =: ("left: " ++ show left ++ "px"))
          (slider, _) <- elDynAttr' "div" sliderAttrs $ text [c]
      (,) <$> mapDyn (`div` 10) dynLeft <*> pure (constDyn $ Marble c)
    mconcatDyn =<< mapM (mapDyn (uncurry Map.singleton)) =<< mapM (uncurry (combineDyn (,))) r
          

test :: MonadWidget t m => m ()
test = do
  rec
    (parent, _) <- elAttr' "div" ("id" =: "parent") $ do
      rec
        mouseDown <- mouseDown_ $ _el_element slider
        mouseUp <- mouseUp_ $ _el_element parent
        isDown <- hold False $ leftmost [const False <$> mouseUp, const True <$> mouseDown]
        evMove <- mouseMove_ $ _el_element parent
        (slider, _) <- elDynAttr' "div" sliderAttrs $ pure ()
        sliderAttrs <- forDyn dynLeft $ \left -> ("id" =: "slider" <> "style" =: ("left: " ++ show (left * 10) ++ "px"))
        dynLeft <- mapDyn fst =<< (holdDyn (0,0) $ gate isDown evMove)
      return ()
  return ()

renderMarbleBehaviour :: (MonadWidget t m)
                      => (Dynamic t (Map Time Marble))
                      -> m (Dynamic t (Map Time Marble))
renderMarbleBehaviour marblesDyn = do
  divClass "marbleBehaviour" $ do
    divClass "timeline" $ pure ()
    marblesList <- mapDyn (map ((\(ts, m:_) -> ((head ts, length ts) , m)) . unzip) . groupBy ((==) `on` snd) . Map.toList) marblesDyn
    simpleList marblesList $ \msDyn -> do
      dyn =<< (forDyn msDyn $ \((t, l), Marble c) -> do
        elAttr "div" ("class" =: "marble" <> "style" =: ("left: " ++ show (t * 10) ++ "px;" ++ "width: " ++ show (l * 10) ++ "px;")) $ do
          text $ [c])
    return marblesDyn
