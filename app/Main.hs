module Main where

import qualified Data.Text as T
import Retime

main :: IO ()
main = do
  -- TODO: cargs
  input <- getLine
  out <- interpretText (T.pack input)
  print out
