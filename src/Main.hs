module Main where

import Control.Arrow((>>>))
import Control.Monad(liftM)

import Data.Maybe(fromMaybe, listToMaybe)

import System.Environment(getArgs)

import Text.ParserCombinators.Parsec(
  (<|>), (<?>), char, digit, endBy, letter, many, many1, noneOf, oneOf, parse, Parser, satisfy, sepBy, skipMany1, space
  , try
  )

a |> f = f a

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

main :: IO ()
main = do
  args <- getArgs
  let firstArg = args |> (listToMaybe >>> (fromMaybe $ error "You must supply an argument!"))
  putStrLn $ readExpr firstArg

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseTicked <|> parseListForms
  where
    parseString :: Parser LispVal
    parseString =
      do
        quote
        x <- many (noneOf "\"" <|> (slash >> escapeChar))
        quote
        return $ String x
      where
        quote = char '"'

    parseAtom :: Parser LispVal
    parseAtom =
      do
        first <- letter <|> symbol
        rest  <- many $ letter <|> digit <|> symbol
        return $ unravel $ first : rest
      where
        unravel "#t" = Bool True
        unravel "#f" = Bool False
        unravel atom = Atom atom

    parseNumber :: Parser LispVal
    parseNumber = liftM (read >>> Number) $ many1 digit

    parseNumberAlt1 :: Parser LispVal
    parseNumberAlt1 =
      do
        digits <- many1 digit
        return $ digits |> (read >>> Number)

    parseNumberAlt2 :: Parser LispVal
    parseNumberAlt2 = (many1 digit) >>= (read >>> Number >>> return)

    parseList :: Parser LispVal
    parseList = liftM List $ sepBy parseExpr spaces

    parseDottedList :: Parser LispVal
    parseDottedList =
      do
        head <- endBy parseExpr spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail

    parseTicked :: Parser LispVal
    parseTicked =
      do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]

    parseListForms :: Parser LispVal
    parseListForms =
      do
        char '('
        x <- try parseList <|> parseDottedList
        char ')'
        return x

escapeChar :: Parser Char
escapeChar = oneOf "\"\r\t\\"

spaces :: Parser ()
spaces = skipMany1 space

slash :: Parser Char
slash = char '\\'

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
