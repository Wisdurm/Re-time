module Main where

import qualified Data.Text as T
import Retime.Tokenizer (tokenize)
import Retime.Parser (parse)

main :: IO ()
main = do
  input <- getLine
  let out = parse . tokenize . T.pack $ input
  print out
