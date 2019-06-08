{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}

module Helper where

import           Data.Coerce                    (Coercible)
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





data HtmlTag ret where
  TagDiv :: HtmlTag HTMLDivElement
  TagSpan :: HtmlTag HTMLSpanElement


class Tag ret tag | tag -> ret where
  tagName :: tag -> String
  tagCast :: (IsGObject ret, Coercible obj JSVal) => tag -> obj -> ret


instance Tag ret (HtmlTag ret) where
  tagName TagDiv  = "div"
  tagName TagSpan  = "span"
  tagCast TagDiv obj  = uncheckedCastTo HTMLDivElement obj
  tagCast TagSpan obj  = uncheckedCastTo HTMLSpanElement obj



createElement' :: (Tag ret (HtmlTag ret), IsGObject ret) =>
                  HTMLDocument -> HtmlTag ret -> JSM ret
createElement' doc tag = do
  el <- createElement doc (tagName tag)
  return $ tagCast tag el


test1 :: JSM HTMLSpanElement
test1 = do
  let doc :: HTMLDocument
      doc = undefined
  -- createElement' doc TagDiv
  createElement' doc TagSpan
