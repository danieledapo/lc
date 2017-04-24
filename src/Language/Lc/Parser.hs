-- | Parse the canonical syntax of lambda calculus
-- additional syntax is planned(like assignement), but not
-- yet implemented
module Language.Lc.Parser
  ( expr
  , line
  , parse
  , parseTest
  , program
  ) where

import Control.Applicative (empty)
import Control.Monad (void)

import Text.Megaparsec hiding (space)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

import Language.Lc

-- Plumbing
-- consume any whitespace-like things(i.e. comments)
space :: Parser ()
space = L.space (void spaceChar) lineComment empty
  where
    lineComment = L.skipLineComment "#"

-- a lexeme p is just p optionally followed by any spaces
lexeme = L.lexeme space

-- a given string optionally followed by any spaces
symbol = L.symbol space

-- include a given parser into parens
parens = between (symbol "(") (symbol ")")

iden :: Parser String
iden = lexeme $ some alphaNumChar

-- Main parser
-- | parse an entire program, that is multiple `line`
program :: Parser [Lc]
program = many line <* eof

-- | parse a single line, that is an `expr` preceded by spaces
line :: Parser Lc
line = space >> expr

-- | parse a lambda expression consuming any following spaces
expr :: Parser Lc
expr = do
  terms <- some term

  -- collect all terms in the expression and bundle them in applications
  -- example: x y z -> [Var "x", Var "y", Var "z"] -> App (App (Var "x") (Var "y")) (Var "z")
  return $ foldl1 (\acc x -> LcApp (App acc x)) terms
  where
    term = parens expr <|> abstraction <|> var

var :: Parser Lc
var = fmap (LcVar . Var) iden

abstraction :: Parser Lc
abstraction = do
  void $ symbol [lam]
  args <- some iden
  void $ symbol "."
  body <- expr

  -- desugar multiple arguments to nested abstractions
  -- example: λx y z.x -> Abs "x" (Abs "y" (Abs "z" (Var "x")))
  let foldAbs arg (acc, isLast) =
        ( LcAbs $ Abs
            arg
            (if isLast
               then body
               else acc)
        , False)
  let explodeAbs = fst $ foldr foldAbs (undefined, True) args
  return explodeAbs
