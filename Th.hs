-- {-# LANGUAGE TemplateHaskell #-}

-- module Th where

-- import Debug.Trace
-- import Language.Haskell.TH

-- import Language.Haskell.TH.Quote

-- (.>) = flip (.); infixl 9 .>
-- ($>) = flip ($); infixl 0 $>

-- -- dump :: ExpQ -> ExpQ
-- -- dump :: Q Exp -> String
-- dump =  QuasiQuoter { quoteExp = parseExprExp, quotePat = undefined }

-- parseExprExp :: String -> Q Exp
-- parseExprExp str = traceShow str $ undefined
--      -- traceShow () $ tuple >>= getElems .> map dumpExpr .> listE
--   -- where
--   --   getElems :: Exp -> [Exp]
--   --   -- getElems = \case { TupE xs -> xs; _ -> error "not a tuple in splice!" }
--   --   getElems (TupE xs) = xs
--   --   getElems _ = error "not a tuple in splice!"

--   --   dumpExpr :: Exp -> ExpQ
--   --   dumpExpr exp = [| $(litE (stringL (pprint exp))) ++ " = " ++ show $(return exp)|]


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

-- fromRight :: Either l r -> r
-- fromRight (Right r) = r
-- fromRight l = error l


-- ["a_1627423681 = True","GHC.Types.False = False"]
-- parser :: GenParser Char st String
parser = identifier >>>= eof

identifier = try variable <|> value

variable = manyTill anyChar (try $ char '_') >>>= many digit

-- number = choice $ map char ['0'..'9']

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
