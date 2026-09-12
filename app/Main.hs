module Main where

import Interpreter (easyInterpret)

main :: IO ()
main = do
  input <- getLine
  let out = easyInterpret input
  putStrLn out
