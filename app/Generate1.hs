{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Generate1 (mkTagData, mkTagInstance, autoTags) where

import           Data.Char           (toUpper)
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




tTag' :: Name
tTag' = mkName "Tag"
tHtmlTag' :: Name
tHtmlTag' = mkName "HtmlTag"
tagName' :: Name
tagName' = mkName "tagName"
tagCast' :: Name
tagCast' = mkName "tagCast"


mkTagInstance :: [String] -> Q [Dec]
mkTagInstance ns = return $ [
  InstanceD Nothing [] (AppT (AppT (ConT tTag') (VarT retName))
                        (AppT (ConT tHtmlTag') (VarT retName)))
    [FunD tagName' (map tagNamed ns),
     FunD tagCast' (map tagCastd ns)]
  ]
  where retName = mkName "ret"
        objName = mkName "obj"
        upper s = map toUpper s
        el t = mkName $ "GHCJS.DOM.Types.HTML" ++ t ++ "Element"
        cnst t = mkName $ "Tag" ++ t
        tagNamed n = Clause [ConP (cnst n) []]
                     (NormalB (LitE (StringL (upper n)))) []
        tagCastd n = Clause [ConP (cnst n) [],VarP objName]
                     (NormalB (AppE (AppE (VarE 'uncheckedCastTo)
                                      (ConE (el n)))
                                (VarE objName))) []
-- _test2 :: IO [Dec]
-- _test2 = runQ [d|
--                instance Tag ret (HtmlTag ret) where
--                  tagName TagDiv  = "DIV"
--                  tagName TagSpan = "SPAN"
--                  tagCast TagDiv obj  = uncheckedCastTo HTMLDivElement obj
--                  tagCast TagSpan obj = uncheckedCastTo HTMLSpanElement obj
--                 |]
