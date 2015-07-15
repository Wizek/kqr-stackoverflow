{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Th where

import Language.Haskell.TH
import Text.Regex

(.>) = flip (.); infixl 9 .>
($>) = flip ($); infixl 0 $>

dump :: ExpQ -> ExpQ
dump tuple =
    listE . map dumpExpr . getElems =<< tuple
  where
    getElems = \case { TupE xs -> xs; _ -> error "not a tuple in splice!" }
    dumpExpr exp = [| $(litE (stringL (ppr exp $> show $> simplify))) ++ " = " ++ show $(return exp)|]

simplify :: String -> String
simplify s = subRegex (mkRegex "_[0-9]+|([a-zA-Z]+\\.)*") s ""
