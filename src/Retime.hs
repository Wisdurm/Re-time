module Retime (interpretText, interpretTextWithSym) where

import qualified Data.Text as T
import Retime.Libraries as Sym
import Retime.Interpreter as Intp
import Retime.Interpreter.Convert as Intp
import Retime.Interpreter.Types as Intp
import Retime.Parser as Ast
import Retime.Tokenizer as Token
import Data.IORef

interpretText :: T.Text -> IO String
interpretText input = do
  sym <- Sym.defaultSymtab
  st <- Intp.defaultArgState
  case Ast.parse (Token.tokenize input) of
    Nothing -> return ""
    (Just ast) -> do
      res <- Intp.interpret ast sym st True
      str <- Intp.debugP res
      return str

interpretTextWithSym :: IORef Symtab -> T.Text -> IO String
interpretTextWithSym symRef input = do
  st <- Intp.defaultArgState
  case Ast.parse (Token.tokenize input) of
    Nothing -> return ""
    (Just ast) -> do
      res <- Intp.interpret ast symRef st True
      str <- Intp.debugP res
      return str
