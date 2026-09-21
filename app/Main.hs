module Main where

import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Retime

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      -- TODO: cargs
      input <- getLine
      out <- interpretText (T.pack input)
      print out
    (file:_) -> do
      contents <- T.readFile file
      out <- interpretText contents
      print out
