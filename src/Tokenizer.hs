module Tokenizer where
import qualified Data.Text as T
import Data.Char (isAlpha, isDigit)

data Token = Identifier T.Text | Literal T.Text | Number T.Text | Punctuation T.Text
  deriving (Show)

tokenize :: T.Text -> [Token]
tokenize str =
  case T.uncons stripped of
    Nothing -> []
    Just (x,xs)
      | isAlpha x -> let (f,s) = splitBy isAlpha stripped
                     in Identifier f : (tokenize . T.strip $ s)
      | isDigit x -> let (f,s) = splitBy isDigit stripped
                     in Number f : (tokenize . T.strip $ s)
      | x == '\"' -> let (f,s) = splitBy (/='\"') xs
                     in Literal f : (tokenize . T.strip . T.tail $ s)
      | elem x "()" -> Punctuation (T.singleton x) : (tokenize . T.strip $ xs)
      | otherwise -> []
  where stripped = T.strip str

splitBy :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
splitBy f str = (T.takeWhile f str, T.dropWhile f str)
