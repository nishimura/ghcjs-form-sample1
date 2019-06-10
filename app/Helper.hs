{-# LANGUAGE RankNTypes #-}
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


getElement :: (Tag ret (HtmlTag ret), IsGObject ret, ToJSString id, Show id, MonadJSM m) =>
                            HTMLDocument -> id -> HtmlTag ret -> m ret
getElement doc eid tag = do
  optEl <- getElementById doc eid
  case optEl of
    Just el -> return $ tagCast tag el
    Nothing -> error $
      "not found: id [" ++ (show eid) ++ "] tagName [" ++ (tagName tag) ++ "]"


data HtmlTag ret where
  TagDiv :: HtmlTag HTMLDivElement
  TagSpan :: HtmlTag HTMLSpanElement
  TagForm :: HtmlTag HTMLFormElement
  TagInput :: HtmlTag HTMLInputElement
  TagButton :: HtmlTag HTMLButtonElement


class Tag ret tag | tag -> ret where
  tagName :: tag -> String
  tagCast :: (IsGObject ret, Coercible obj JSVal) => tag -> obj -> ret


instance Tag ret (HtmlTag ret) where
  tagName TagDiv  = "div"
  tagName TagSpan  = "span"
  tagName TagForm = "form"
  tagName TagInput = "input"
  tagName TagButton = "button"
  tagCast TagDiv obj  = uncheckedCastTo HTMLDivElement obj
  tagCast TagSpan obj  = uncheckedCastTo HTMLSpanElement obj
  tagCast TagForm obj  = uncheckedCastTo HTMLFormElement obj
  tagCast TagInput obj  = uncheckedCastTo HTMLInputElement obj
  tagCast TagButton obj  = uncheckedCastTo HTMLButtonElement obj



createElement' :: (Tag ret (HtmlTag ret), IsGObject ret, MonadJSM m) =>
                  HTMLDocument -> HtmlTag ret -> m ret
createElement' doc tag = do
  el <- createElement doc (tagName tag)
  return $ tagCast tag el


test1 :: JSM HTMLSpanElement
test1 = do
  let doc :: HTMLDocument
      doc = undefined
  -- createElement' doc TagDiv
  createElement' doc TagSpan
