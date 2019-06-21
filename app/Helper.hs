{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Helper(
  Tag(..)
  , HtmlTag(..)
  , (<?>)
  , getElementById'
  , getElement
  , createElement'
)where

import           Control.Monad.Trans.Except     (ExceptT (..))
import           Control.Monad.Trans.Maybe      (MaybeT (..), maybeToExceptT)
import           Generate2
import           GHCJS.DOM.Document             (createElement)
import           GHCJS.DOM.Element              (getTagName)
import           GHCJS.DOM.NonElementParentNode (getElementById)
import           GHCJS.DOM.Types


$(mkTagInstance autoTags)


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



createElement' :: (Tag ret (HtmlTag ret), IsGObject ret, MonadJSM m) =>
                  HTMLDocument -> HtmlTag ret -> m ret
createElement' doc tag = do
  el <- createElement doc (tagName tag)
  return $ tagCast tag el


_test1 :: JSM HTMLSpanElement
_test1 = do
  let doc :: HTMLDocument
      doc = undefined
  -- createElement' doc TagDiv
  createElement' doc TagSpan
