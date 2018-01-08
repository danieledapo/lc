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


-- | parse an expression, a 'let_' or 'lc'
expr :: P.Parser ELc
expr = try (ELcLet <$> let_) <|> lc

-- | parse a normal 'lc'
lc :: P.Parser ELc
lc = ELc <$> P.expr

-- | parse a 'let_' like `a = 42`
let_ :: P.Parser Let
let_ = do
  name <- P.iden
  void $ P.symbol "="
  value <- P.expr
  return . Let name $ value
