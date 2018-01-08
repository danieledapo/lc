-- | Parse the canonical syntax of lambda calculus
-- additional syntax is planned(like assignement), but not
-- yet implemented
module Language.Lc.Parser
  ( Parser
  , expr
  , iden
  , lexeme
  , parens
  , space
  , symbol
  ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char (char, spaceChar, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Lc

--------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------

-- | Type for any lc parser
type Parser = Parsec Void String

-- | consume any whitespace-like things(i.e. comments)
space :: Parser ()
space = L.space (void spaceChar) lineComment empty
 where
  lineComment :: Parser ()
  lineComment = L.skipLineComment "#"

-- | a lexeme p is just p optionally followed by any spaces
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

-- | a given string optionally followed by any spaces
symbol :: String -> Parser String
symbol = L.symbol space

-- | include a given parser into parens
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | identifier
iden :: Parser String
iden = lexeme . some $ alphaNumChar <|> char '_'


--------------------------------------------------------------
-- Main parser
--------------------------------------------------------------


-- | parse a lambda expression consuming any following spaces
expr :: Parser Lc
expr = do
  terms <- some term

  -- collect all terms in the expression and bundle them in applications
  -- example: x y z -> [LcVar "x", LcVar "y", LcVar "z"] -> LcApp (LcApp (LcVar "x") (LcVar "y")) (LcVar "z")
  return $ foldl1 LcApp terms
  where term = parens expr <|> abstraction <|> var

-- | parse a 'LcVar'
var :: Parser Lc
var = LcVar <$> iden

-- | parse a 'LcAbs'
abstraction :: Parser Lc
abstraction = do
  void $ symbol [lam] <|> symbol "\\"
  args <- some iden
  void $ symbol "."
  body <- expr

  -- desugar multiple arguments to nested abstractions
  -- example: λx y z.x -> Abs "x" (Abs "y" (Abs "z" (Var "x")))
  return $ foldr LcAbs body args
