{-# LANGUAGE OverloadedStrings #-}
module Retime.Parser (parse, Ast(..)) where

import Retime.Tokenizer as Token (Token(..))
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.Read as T (double)

data Ast = Call {callObject :: Ast, callArgs :: [Ast]}
         | Literal (Either T.Text Double)
         | Identifier T.Text
         deriving (Show, Eq)
type Pos = Int

parse :: [Token] -> Maybe Ast
parse xs = let (ast,_) = runState (parse' xs) 0
           in ast

parse' :: [Token] -> State Pos (Maybe Ast)
parse' ((Token.Identifier obj):(Token.Punctuation "("):rest) = do
  modify (+1)
  args <- getArgs rest
  return . Just $ (Call { callObject = Retime.Parser.Identifier obj,
                          callArgs = args})
  where getArgs [] = return []
        getArgs xs = do
          initial <- get
          res <- parse' xs
          later <- get
          case res of
            Nothing -> return []
            Just val -> do
              rest <- getArgs (drop (later-initial) xs)
              return (val : rest)
parse' ((Token.Identifier str):_) = do
  modify (+1)
  return . Just . Retime.Parser.Identifier $ str
parse' ((Token.Literal str):_) = do
  modify (+1)
  return . Just . Retime.Parser.Literal . Left $ str
parse' ((Token.Number str):_) = do
  modify (+1)
  case T.double str of
    Left err -> error err
    Right (v,_) -> return . Just .
                   Retime.Parser.Literal . Right $ v
parse' ((Token.Punctuation ")"):_) = do
  modify (+2)
  return Nothing
parse' [] = return Nothing
parse' _ = error "Weird parser behaviour"
