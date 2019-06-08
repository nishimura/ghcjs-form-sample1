{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
module Helper where

import           GHCJS.DOM.Document             (createElement)
import           GHCJS.DOM.NonElementParentNode (getElementById)
import           GHCJS.DOM.Types


getElementById' :: (MonadJSM m, ToJSString elementId) =>
                   HTMLDocument -> elementId -> m (Maybe HTMLElement)
getElementById' doc eid = do
  optEl <- getElementById doc eid
  case optEl of
    Just el -> castTo HTMLElement el
    Nothing -> return Nothing

createElement' :: (MonadJSM m, ToJSString name) =>
                  HTMLDocument -> name -> m HTMLElement
createElement' doc name = do
  el <- createElement doc name
  return $ uncheckedCastTo HTMLElement el
