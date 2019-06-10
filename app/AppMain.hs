{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AppMain where


import           Control.Concurrent.MVar          (newEmptyMVar, putMVar,
                                                   takeMVar)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans.Reader       (ReaderT)

import           GHCJS.DOM                        (currentDocument, syncPoint)
import           GHCJS.DOM.Document               (createTextNode, getBody)
import           GHCJS.DOM.Element                (setAttribute, setInnerHTML)
import           GHCJS.DOM.EventM                 (mouseClientXY, on,
                                                   preventDefault)
import qualified GHCJS.DOM.GlobalEventHandlers    as G (click, submit)
import           GHCJS.DOM.HTMLInputElement       (getValue, setValue)
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
  optArea <- prepare doc
  case optArea of
    Just area -> application doc area
    Nothing   -> return ()

prepare :: HTMLDocument -> JSM (Maybe HTMLElement)
prepare doc = do
  optBody <- getBody doc
  optArea <- case optBody of
    Just _  -> getElementById' doc "area"
    Nothing -> liftIO $ putStrLn "<body> not found" >> return Nothing
  case optArea of
    Just area -> return $ Just area
    Nothing   -> liftIO $ putStrLn "id=\"area\" not found" >> return Nothing




getStyle :: (Show a1, Show a2) => a1 -> a2 -> Bool -> String
getStyle x y input =
  "left:" ++ show x ++ "px; top:" ++ show y ++ "px;"
  ++ if input then "background-color:blue" else "background-color: red"


showPoint :: (IsElement self, IsEvent e, Show a1, Show a2) =>
             HTMLDocument -> self -> (a1, a2) -> Bool -> ReaderT e DOM ()
showPoint doc area (x,y) input = do
  setInnerHTML area ""
  preventDefault
  point <- createElement' doc TagDiv
  setAttribute point "style" $ getStyle x y input
  _ <- appendChild area point
  return ()

application :: HTMLDocument -> HTMLElement -> JSM ()
application doc area = do
    Just body <- getBody doc
    input <- getElement doc "coordinate" TagInput
    releaseClick <- on area G.click $ do
      (x, y) <- mouseClientXY
      setValue input $ show (x, y)
      showPoint doc area (x, y) False

    form <- getElement doc "form" TagForm
    _ <- on form G.submit $ do
      preventDefault
      val::String <- getValue input
      let xy = read val::(Int,Int)
      showPoint doc area xy True

    -- Make an exit button
    exitMVar <- liftIO newEmptyMVar
    exit <- createElement' doc TagButton
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
