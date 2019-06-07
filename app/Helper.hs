module Helper where

import           GHCJS.DOM          (currentDocument)
import           GHCJS.DOM.Document (createElement)
import           GHCJS.DOM.Types


createHTMLElement ::
  (MonadJSM m, ToJSString localName) => localName -> m HTMLElement
createHTMLElement tag = do
  Just doc <- currentDocument
  createHTMLElement' doc tag

createHTMLElement' ::
  (MonadJSM m, IsDocument self, ToJSString localName) =>
  self -> localName -> m HTMLElement
createHTMLElement' doc tag = do
  el <- createElement doc tag
  return $ castHTMLElement el

castHTMLElement :: Element -> HTMLElement
castHTMLElement  = uncheckedCastTo HTMLElement

