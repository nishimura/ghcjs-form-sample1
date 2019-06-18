{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Helper where

import           Control.Monad.Trans.Except     (ExceptT (..))
import           Control.Monad.Trans.Maybe      (MaybeT (..), maybeToExceptT)
import           Data.Coerce                    (Coercible)
import           GHCJS.DOM.Document             (createElement)
import           GHCJS.DOM.Element              (getTagName)
import           GHCJS.DOM.NonElementParentNode (getElementById)
import           GHCJS.DOM.Types


trans :: MonadJSM m => e -> m (Maybe a) -> ExceptT e m a
trans e = (maybeToExceptT e) . MaybeT


(<?>) :: MonadJSM m => m (Maybe a) -> e -> ExceptT e m a
m <?> e = trans e m
infix  0 <?>


($++) :: (ToJSString js, Show js) => js -> String -> String
js $++ s = show js ++ s
infixr 5 $++


errId :: (ToJSString id, Show id) => id -> String
errId jsid = "can not find id=[" ++ show jsid ++ "]"


getElementById' :: (MonadJSM m, ToJSString elementId, Show elementId) =>
                   HTMLDocument -> elementId -> ExceptT String m HTMLElement
getElementById' doc eid = do
  el <- getElementById doc eid <?> errId eid
  castTo HTMLElement el <?> "not cast to HTMLElement id=[" ++ eid $++ "]"


getElement :: (Tag ret (HtmlTag ret), IsGObject ret, ToJSString id, Show id, MonadJSM m) =>
                            HTMLDocument -> id -> HtmlTag ret -> ExceptT String m ret
getElement doc eid tag = do
  el <- getElementById doc eid <?> "cannot"
  let t = tagCast tag el
      n = tagName tag
  tn <- getTagName el
  if n == tn
    then return t
    else error $ "not found: id [" ++ eid $++ "] tagName [" ++ n ++ "]"


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
  tagName TagDiv    = "DIV"
  tagName TagSpan   = "SPAN"
  tagName TagForm   = "FORM"
  tagName TagInput  = "INPUT"
  tagName TagButton = "BUTTON"
  tagCast TagDiv obj    = uncheckedCastTo HTMLDivElement obj
  tagCast TagSpan obj   = uncheckedCastTo HTMLSpanElement obj
  tagCast TagForm obj   = uncheckedCastTo HTMLFormElement obj
  tagCast TagInput obj  = uncheckedCastTo HTMLInputElement obj
  tagCast TagButton obj = uncheckedCastTo HTMLButtonElement obj



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
