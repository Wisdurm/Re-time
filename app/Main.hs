module Main where

import qualified Data.Text as T
import Retime.Tokenizer
import Retime.Parser
import Retime.Interpreter

main :: IO ()
main = do
  input <- getLine
  case parse . tokenize . T.pack $ input of
    Nothing -> return ()
    Just ast -> do
      sym <- defaultSymtab
      interpret ast sym True >>= debugP >>= print
