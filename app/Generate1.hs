{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Generate1 (mkTagData, autoTags) where

import           GHCJS.DOM.Types
import           Language.Haskell.TH

-- data HtmlTag ret where
--   TagDiv :: HtmlTag HTMLDivElement
--   TagSpan :: HtmlTag HTMLSpanElement


mkTagData :: [String] -> Q [Dec]
mkTagData ns = return $ [DataD [] name [PlainTV (mkName "ret")] Nothing
                         (map mkdata ns)
                         []
                        ]
  where name = mkName "HtmlTag"
        el t = mkName $ "GHCJS.DOM.Types.HTML" ++ t ++ "Element"
        mkdata n = GadtC [mkName ("Tag" ++ n)] [] (AppT (ConT name) (ConT (el n)))


autoTags :: [String]
autoTags = [
  "Div"
  , "Span"
  , "Form"
  , "Input"
  , "Button"
  ]




_test1 :: IO [Dec]
_test1 = runQ [d|
               data HtmlTag ret where
                  TagDiv :: HtmlTag HTMLDivElement
                  TagSpan :: HtmlTag HTMLSpanElement
                |]

