module Main where
import qualified Data.Text as T

import Tokenizer

main :: IO ()
main = do
  input <- getLine
  let out = tokenize . T.pack $ input
  print out
