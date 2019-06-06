{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}
module FormSample where

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.HTMLCollection
import           GHCJS.DOM.Types

formEl :: MaybeT JSM Element
formEl = do
  doc <- MaybeT currentDocument
  forms <- getForms doc
  form <- MaybeT $ namedItem forms "form"
  return form

foo = undefined
debug :: JSM ()
debug = do
  mform <- runMaybeT formEl
  case mform of
    Just a  -> foo a
    Nothing -> undefined
  return ()


#ifdef MIN_VERSION_ghcjs_dom_jsffi

runForm :: IO ()
runForm = debug

#else


runForm :: IO ()
runForm =  runJSM debug ctx
  where ctx :: JSContextRef
        ctx = undefined

#endif
