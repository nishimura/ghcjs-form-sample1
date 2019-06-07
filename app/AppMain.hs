{-# LANGUAGE CPP #-}

module AppMain where


import           Control.Concurrent.MVar          (newEmptyMVar, putMVar,
                                                   takeMVar)
import           Control.Monad.IO.Class           (MonadIO (..))

import           GHCJS.DOM                        (currentDocument, syncPoint)
import           GHCJS.DOM.Document               (createElement,
                                                   createTextNode, getBody)
import           GHCJS.DOM.Element                (setInnerHTML)
import           GHCJS.DOM.EventM                 (EventM (..), mouseClientXY,
                                                   on)
import           GHCJS.DOM.EventTargetClosures    (EventName (..))
import qualified GHCJS.DOM.GlobalEventHandlers    as G (click)
import qualified GHCJS.DOM.HTMLElement            (toHTMLElement)
import           GHCJS.DOM.Node                   (appendChild)
import           GHCJS.DOM.NonElementParentNode   (getElementById)
import           GHCJS.DOM.Types

#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp (run)
#endif

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
  return $ toHtmlElement el

toHtmlElement :: Element -> HTMLElement
toHtmlElement  = uncheckedCastTo HTMLElement


#ifdef __GHCJS__

appMain :: IO ()
appMain = do
  Just doc <- currentDocument
  app doc

#else

indexhtml = "tmpl/index.html"

appMain :: IO ()
appMain = run 8000 $ do
  Just doc <- currentDocument
  Just body <- getBody doc
  html <- liftIO $ readFile indexhtml
  setInnerHTML body html
  app doc

#endif


app :: Document -> JSM ()
app doc = do
    Just body <- getBody doc
    Just _area <- getElementById doc "area"
    let area = toHtmlElement _area
    releaseClick <- on area G.click $ do
        (x, y) <- mouseClientXY
        newParagraph <- createElement doc "p"
        text <- createTextNode doc $ "Click " ++ show (x, y)
        appendChild newParagraph text
        appendChild body newParagraph
        return ()

    -- Make an exit button
    exitMVar <- liftIO newEmptyMVar
    exit <- createHTMLElement "span"
    text <- createTextNode doc "Click here to exit"
    appendChild exit text
    appendChild body exit
    releaseExit <- on exit G.click $ liftIO $ putMVar exitMVar ()

    -- Force all all the lazy evaluation to be executed
    syncPoint

    -- In GHC compiled version the WebSocket connection will end when this
    -- thread ends.  So we will wait until the user clicks exit.
    liftIO $ takeMVar exitMVar
    releaseClick
    releaseExit
    setInnerHTML body "<h1>Ka kite ano (See you later)</h1>"
    return ()
