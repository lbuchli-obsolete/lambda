module Parser where

import Control.Applicative (many, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Control.Monad (void)
import Text.Parsec.Error()
import Text.Parsec (parse, parseTest, optional, manyTill, many1, try, notFollowedBy, eof)
import Text.Parsec.Prim ((<?>))
import Data.Bifunctor (first)
import Language
import Util

parse :: String -> String -> Result Prog
parse name inp = to_res (first print $ Text.Parsec.parse prog name inp)
  where
    to_res (Left msg) = Error msg
    to_res (Right p)  = Success p

testParse :: String -> IO ()
testParse = parseTest (test_parser <* optional ws <* eof)
  where
    test_parser = prog
  
prog :: Parser Prog
prog = Prog <$> many def <* eof

def :: Parser Def
def = (Def <$> ((,) <$> name <*> expr <* newline))
    <|> manyTill anyChar newline *> def
  where
    name = str "::" *> symbol <* str "="

expr :: Parser Expr
expr = try edef
     <|> (mkApChain EAp <$> many1 (paren <|> var))
  where
    edef  = EDef <$> name <*> expr
    name  = oneOf "\\λ" *> symbol <* str "."
    paren = str "(" *> expr <* str ")"
    var   = EVar <$> symbol

symbol :: Parser Symbol
symbol = many1 possible_char <* optional ws
  where
    possible_char = notFollowedBy (oneOf "\\λ(). \t\n") *> anyChar

str :: String -> Parser ()
str s = void (string s *> optional ws)

ws :: Parser ()
ws = void (many1 (oneOf "\t ")) <?> "whitespace"

mkApChain :: (a -> a -> a) -> [a] -> a
mkApChain f (x:xs) | not (null xs) = f x (mkApChain f xs)
mkApChain _ (x:xs) | null xs       = x
mkApChain _ _                      = error "Empty expression"
