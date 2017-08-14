-- | parse an 'ELc'
module Language.ELc.Parser
  ( expr
  , lc
  , let_
  ) where

import Control.Monad (void)

import Language.ELc
import qualified Language.Lc.Parser as P

import Text.Megaparsec
import Text.Megaparsec.String


-- | parse an expression, a 'let_' or 'lc'
expr :: Parser ELc
expr = try (ELcLet <$> let_) <|> lc

-- | parse a normal 'lc'
lc :: Parser ELc
lc = ELc <$> P.expr

-- | parse a 'let_' like `a = 42`
let_ :: Parser Let
let_ = do
  name <- P.iden
  void $ P.symbol "="
  value <- P.expr
  return . Let name $ value
