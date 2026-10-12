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

-- | Parses a series of tokens into an ast tree
parse :: [Token] -> Maybe Ast
parse xs = let (ast,_) = runState (parse') xs
           in ast

-- Not good code but it was even worse before!

parse' :: State [Token] (Maybe Ast)
parse' = do
  tokens <- get
  modify (drop 1)
  case tokens of
    [] -> return Nothing
    (token:_) -> case parseToken token of
      Nothing -> return Nothing
      t -> capparse t

capparse :: Maybe Ast -> State [Token] (Maybe Ast)
capparse token = do
  tokens <- get
  case tokens of
    (Token.Punctuation "(":_) -> do
      modify (drop 1)
      args <- let getArgs  = do
                    res <- parse'
                    case res of
                      Nothing -> return []
                      Just val -> do
                        rest <- getArgs
                        return (val : rest)
              in getArgs
      capparse (Just $ (Call { callObject = maybe undefined id token,
                               callArgs = args}))
    _ -> return token

parseToken :: Token -> Maybe Ast
parseToken token = case token of
  (Token.Identifier str) -> Just . Retime.Parser.Identifier $ str
  (Token.Literal str) -> Just . Retime.Parser.Literal . Left $ str
  (Token.Number str) -> case T.double (T.replace "," "." str) of
                          Left err -> error err
                          Right (v,_) -> Just . Retime.Parser.Literal . Right $ v
  _ -> Nothing
