module Utils.Parser where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad ((>=>))
import Data.Char (isDigit, isLetter, isSpace)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f a = Parser (runParser a >=> \(x, input') -> Just (f x, input'))

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser a) <*> (Parser b) = Parser (a >=> (\(f, input') -> b input' >>= \(x, input'') -> Just (f x, input'')))
  (Parser a) *> (Parser b) = Parser (a >=> (\(x, input') -> b input'))
  (Parser a) <* (Parser b) = Parser (a >=> (\(x, input') -> b input' >>= \(y, input'') -> Just (x, input'')))

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f [] = Nothing
    f (y : ys) = if y == x then Just (x, ys) else Nothing

-- need to glue multiple integer parsers together and parse out the commas
naturalNumParser :: Parser Int
naturalNumParser = Parser $ \input -> let (token, rest) = span isDigit input in if null token then Nothing else Just (read token, rest)

intParser :: Parser Int
intParser = Parser $ \input -> let (token, rest) = span (== '-') input in if not (null token) then runParser (negate <$> naturalNumParser) rest else runParser naturalNumParser input

alphaNumStringParser :: Parser String
alphaNumStringParser = Parser $ \input -> let (token, rest) = span (\char -> isLetter char || isDigit char) input in if null token then Nothing else Just (token, rest)

stringParser :: Parser String
stringParser = Parser $ \input -> let (token, rest) = span (\char -> isLetter char) input in if null token then Nothing else Just (token, rest)

wsParser :: Parser String
wsParser = Parser $ \input -> let (token, rest) = span isSpace input in Just (token, rest)

sepByParser :: Parser a -> Parser b -> Parser [b]
sepByParser sep value = (:) <$> value <*> many (sep *> value) <|> pure []