module Tokenizer where

data TokenType = TIdentifier | TString | TNumber | TPunctuation
  deriving Show

data Token = Token { ttext :: String, ttype :: TokenType }
  deriving Show

characters :: [Char]
characters = "qwertyuiopasdfghjklzxcvbnmQWERTYUIPOASDFGHJKLZXCVBNM+"

punctuation :: [Char]
punctuation = "()"

numbers :: [Char]
numbers = "0123456789"

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
  | elem x characters = split (x:xs) characters TIdentifier
  | elem x punctuation = split (x:xs) punctuation TPunctuation
  | elem x numbers = split (x:xs) numbers TNumber
  | x == '"' = Token { ttext = takeWhile (/='"') xs, ttype = TString }
               : tokenize (tail . dropWhile (/='"') $ xs)
  | otherwise = tokenize xs
  
split :: String -> String -> TokenType -> [Token]
split (x:xs) set t = Token {
  ttext = takeWhile (\c -> elem c set) (x:xs),
  ttype = t
  } : tokenize (dropWhile (\c -> elem c set) xs)
