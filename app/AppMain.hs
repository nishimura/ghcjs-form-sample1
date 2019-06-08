{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AppMain where


import           Control.Concurrent.MVar          (newEmptyMVar, putMVar,
                                                   takeMVar)
import           Control.Monad.IO.Class           (MonadIO (..))

import           GHCJS.DOM                        (currentDocument, syncPoint)
import           GHCJS.DOM.Document               (createElement,
                                                   createTextNode, getBody)
import           GHCJS.DOM.Element                (setInnerHTML)
import           GHCJS.DOM.EventM                 (mouseClientXY, on)
import qualified GHCJS.DOM.GlobalEventHandlers    as G (click)
import           GHCJS.DOM.Node                   (appendChild)
import           GHCJS.DOM.Types

import           Helper


#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp (run)
#endif


#ifdef __GHCJS__

appMain :: IO ()
appMain = do
  Just doc <- currentDocument
  app $ uncheckedCastTo HTMLDocument doc

#else

indexhtml :: String
indexhtml = "tmpl/index.html"

appMain :: IO ()
appMain = do
  putStrLn "start server: http://localhost:8000"
  run 8000 $ do
    Just doc <- currentDocument
    Just body <- getBody doc
    html <- liftIO $ readFile indexhtml
    setInnerHTML body html
    app $ uncheckedCastTo HTMLDocument doc

#endif


app :: HTMLDocument -> JSM ()
app doc = do
    Just body <- getBody doc
    Just area <- getElementById' doc "area"
    releaseClick <- on area G.click $ do
        (x, y) <- mouseClientXY
        newParagraph <- createElement doc "p"
        text <- createTextNode doc $ "Click " ++ show (x, y)
        _ <- appendChild newParagraph text
        _ <- appendChild body newParagraph
        return ()

    -- Make an exit button
    exitMVar <- liftIO newEmptyMVar
    exit <- createElement' doc "span"
    text <- createTextNode doc "Click here to exit"
    _ <- appendChild exit text
    _ <- appendChild body exit
    releaseExit <- on exit G.click $ liftIO $ putMVar exitMVar ()

    -- Force all all the lazy evaluation to be executed
    syncPoint

    -- In GHC compiled version the WebSocket connection will end when this
    -- thread ends.  So we will wait until the user clicks exit.
    _ <- liftIO $ takeMVar exitMVar
    releaseClick
    releaseExit
    setInnerHTML body "<h1>Ka kite ano (See you later)</h1>"
    return ()
