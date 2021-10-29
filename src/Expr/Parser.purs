module Expr.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy as Lazy
import Data.Either (Either)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser as StringParser
import Text.Parsing.StringParser.CodePoints as CodePoints
import Text.Parsing.StringParser.Combinators as Combinators

-- expr   ::= term + expr | term
-- term   ::= factor * term | factor
-- factor ::= power ** factor | power
-- power  ::= (expr) | a
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Pow Expr Expr
  | Lit Int

instance Show Expr where
  show = case _ of
    Add x y -> "(Add " <> show x <> " " <> show y <> ")"
    Mul x y -> "(Mul " <> show x <> " " <> show y <> ")"
    Pow x y -> "(Pow " <> show x <> " " <> show y <> ")"
    Lit x -> show x

expr :: Unit -> Parser Expr
expr _ =
  StringParser.try
    ( do
        _ <- CodePoints.skipSpaces
        x <- Lazy.defer term
        _ <- CodePoints.skipSpaces
        _ <- CodePoints.string "+"
        _ <- CodePoints.skipSpaces
        y <- Lazy.defer expr
        _ <- CodePoints.skipSpaces
        pure (Add x y)
    )
    <|> Lazy.defer term

term :: Unit -> Parser Expr
term _ =
  StringParser.try
    ( do
        _ <- CodePoints.skipSpaces
        x <- Lazy.defer factor
        _ <- CodePoints.skipSpaces
        _ <- CodePoints.string "*"
        _ <- CodePoints.skipSpaces
        y <- Lazy.defer term
        _ <- CodePoints.skipSpaces
        pure (Mul x y)
    )
    <|> Lazy.defer factor

factor :: Unit -> Parser Expr
factor _ =
  StringParser.try
    ( do
        _ <- CodePoints.skipSpaces
        x <- Lazy.defer power
        _ <- CodePoints.skipSpaces
        _ <- CodePoints.string "**"
        _ <- CodePoints.skipSpaces
        y <- Lazy.defer factor
        _ <- CodePoints.skipSpaces
        pure (Pow x y)
    )
    <|> Lazy.defer power

power :: Unit -> Parser Expr
power _ =
  StringParser.try
    ( Combinators.between
        (CodePoints.string "(")
        (CodePoints.string ")")
        (CodePoints.skipSpaces *> Lazy.defer expr <* CodePoints.skipSpaces)
    )
    <|> lit

lit :: Parser Expr
lit = do
  d <- CodePoints.regex "\\d"
  case Int.fromString d of
    Just d' -> pure (Lit d')
    _ -> StringParser.fail "Not an Int somehow"

run :: String -> Either { error :: String, pos :: Int } Expr
run = StringParser.runParser (expr unit <* CodePoints.eof)
