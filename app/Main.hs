module Main where

import qualified Data.Text as T
import Retime.Tokenizer
import Retime.Parser
import Retime.Interpreter
import Data.IORef

main :: IO ()
main = do
  -- TODO: cargs
  carg1 <- newIORef (2 :: Double)
  cargs <- newIORef [Right carg1]
  input <- getLine
  case parse . tokenize . T.pack $ input of
    Nothing -> return ()
    Just ast -> do
      sym <- defaultSymtab
      interpret ast sym cargs True >>= debugP >>= print
