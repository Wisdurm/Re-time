module Retime.Tokenizer (tokenize, Token(..)) where

import qualified Data.Text as T
import Data.Char (isAlpha, isDigit, isSpace)

data Token = Identifier T.Text
           | Literal T.Text
           | Number T.Text
           | Punctuation T.Text
           deriving (Show, Eq)

-- | Turns a string into a list of tokens
tokenize :: T.Text -> [Token]
tokenize str =
  case T.uncons stripped of
    Nothing -> []
    Just (x,xs)
      | x == '#' -> tokenize (T.dropWhile (/='\n') xs)
      | x == '\"' -> let (f,s) = splitBy (/='\"') xs
                     in Literal f : (tokenize . T.strip . T.tail $ s)
      | elem x "()" -> Punctuation (T.singleton x) : (tokenize . T.strip $ xs)
      | isNumber x -> let (f,s) = splitBy isNumber stripped
                      in Number f : (tokenize . T.strip $ s)
      | isIdentifier x -> let (f,s) = splitBy isIdentifier stripped
                          in Identifier f : (tokenize . T.strip $ s)
      | otherwise -> []
  where stripped = T.strip str

-- | Whether a character is one that could be a part of an identifier
isIdentifier :: Char -> Bool
isIdentifier c = (not . isSpace $ c) && (not . isDigit $ c) && (not . elem c $ "()")

-- | Whether a character is part of a number literal
isNumber :: Char -> Bool
isNumber c = isDigit c || elem c ",."

splitBy :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
splitBy f str = (T.takeWhile f str, T.dropWhile f str)
