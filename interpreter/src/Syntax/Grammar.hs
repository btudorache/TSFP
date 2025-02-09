module Syntax.Grammar where

import Control.Applicative
import Data.Char

import Syntax.Expression
import Syntax.Parser

parseProgram :: String -> Maybe [Expression]
parseProgram = parse $ (many ((parseDefinition <|> parseExpression) <* parseSpaces)) <* eof

parseSpaces :: Parser String
parseSpaces = many $ spot isSpace

parseString :: Parser String
parseString = some $ spot isLetter

parseExpression :: Parser Expression
parseExpression = parseLambda <|> parseApplication <|> parseVar

parseVar :: Parser Expression
parseVar = liftA Var parseString

parseDefinition :: Parser Expression
parseDefinition = liftA2 Def parseString (token '=' *> parseExpression) 

parseLambda :: Parser Expression
parseLambda = liftA2 Lambda (token '\\' *> parseString) (token '.' *> parseExpression)

parseApplication :: Parser Expression
parseApplication = liftA2 Application (token '(' *> parseExpression) (parseSpaces *> parseExpression <* token ')')
