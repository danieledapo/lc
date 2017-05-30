-- | parse an 'ELc'
module Language.ELc.Parser where

import Control.Monad (void)

import Language.ELc
import qualified Language.Lc.Parser as P

import Text.Megaparsec
import Text.Megaparsec.String

-- | parse an expression, a 'let_' or 'lc'
expr :: Parser ELc
expr = try let_ <|> lc

-- | parse a normal 'lc'
lc :: Parser ELc
lc = fmap ELc P.expr

-- | parse a 'let_' like `a = 42`
let_ :: Parser ELc
let_ = do
  name <- P.iden
  void $ P.symbol "="
  value <- P.line
  return . ELcLet . Let name $ value
