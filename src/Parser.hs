module Parser
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

import Lib (AST(..), lam)

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
program :: Parser [AST]
program = many line <* eof

line :: Parser AST
line = space >> expr

expr :: Parser AST
expr = do
  terms <- some term
  -- collect all terms in the expression and bundle them in applications
  -- example: x y z -> [Var "x", Var "y", Var "z"] -> App (App (Var "x") (Var "y")) (Var "z")
  return $ foldl1 App terms
  where
    term = parens expr <|> abstraction <|> var

var :: Parser AST
var = Var <$> iden

abstraction :: Parser AST
abstraction = do
  void $ symbol [lam]
  args <- some iden
  void $ symbol "."
  body <- expr
  -- desugar multiple arguments to nested abstractions
  -- example: λx y z.x -> Abs "x" (Abs "y" (Abs "z" (Var "x")))
  let foldAbs arg (acc, isLast) =
        ( Abs
            arg
            (if isLast
               then body
               else acc)
        , False)
  let explodeAbs = fst $ foldr foldAbs (undefined, True) args
  return explodeAbs
