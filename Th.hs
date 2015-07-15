{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Th where

import Language.Haskell.TH
import Text.ParserCombinators.Parsec
import Data.Either
import Debug.Trace

(.>) = flip (.); infixl 9 .>
($>) = flip ($); infixl 0 $>

dump :: ExpQ -> ExpQ
dump tuple =
    listE . map dumpExpr . getElems =<< tuple
  where
    getElems = \case { TupE xs -> xs; _ -> error "not a tuple in splice!" }
    dumpExpr exp = [| $(litE (stringL (simplify $ show $ ppr exp))) ++ " = " ++ show $(return exp)|]

simplify :: String -> String
simplify s = parse parser ("Source: " ++ s) s $> either (show .> error) id

parser = identifier >>>= eof

identifier = try variable <|> value

variable = manyTill anyChar (try $ char '_') >>>= many digit


value = manyTill' anyChar lastBit $> fmap snd
  where
    lastBit = many letter >>>= eof

manyTill' p1 p2 = do
  r1 <- manyTill p1 (lookAhead $ try p2)
  r2 <- p2
  return (r1, r2)

a >>>= b = do
  result <- a
  b
  return result
