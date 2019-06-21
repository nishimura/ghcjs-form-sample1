{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Generate2 (mkTagInstance, HtmlTag(..), autoTags, Tag(..)) where

import           Data.Char           (toUpper)
import           Data.Coerce         (Coercible)
import           Generate1
import           GHCJS.DOM.Types
import           Language.Haskell.TH


$(mkTagData autoTags)


class Tag ret tag | tag -> ret where
  tagName :: tag -> String
  tagCast :: (IsGObject ret, Coercible obj JSVal) => tag -> obj -> ret

-- instance Tag ret (HtmlTag ret) where
--   tagName TagDiv    = "DIV"
--   tagCast TagDiv obj  = uncheckedCastTo HTMLDivElement obj
--   tagCast TagSpan obj = uncheckedCastTo HTMLSpanElement obj



mkTagInstance :: [String] -> Q [Dec]
mkTagInstance ns = return $ [
  InstanceD Nothing [] (AppT (AppT (ConT ''Tag) (VarT retName))
                        (AppT (ConT ''HtmlTag) (VarT retName)))
    [FunD 'tagName (map tagNamed ns),
     FunD 'tagCast (map tagCastd ns)]
  ]
  where retName = mkName "ret"
        objName = mkName "obj"
        upper s = map toUpper s
        el t = mkName $ "GHCJS.DOM.Types.HTML" ++ t ++ "Element"
        cnst t = mkName $ "Generate2.Tag" ++ t
        tagNamed n = Clause [ConP (cnst n) []]
                     (NormalB (LitE (StringL (upper n)))) []
        tagCastd n = Clause [ConP (cnst n) [],VarP objName]
                     (NormalB (AppE (AppE (VarE 'uncheckedCastTo)
                                      (ConE (el n)))
                                (VarE objName))) []


_test1 :: IO [Dec]
_test1 = runQ [d|
               instance Tag ret (HtmlTag ret) where
                 tagName TagDiv  = "DIV"
                 tagName TagSpan = "SPAN"
                 tagCast TagDiv obj  = uncheckedCastTo HTMLDivElement obj
                 tagCast TagSpan obj = uncheckedCastTo HTMLSpanElement obj
                |]

