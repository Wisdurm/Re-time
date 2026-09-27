module Retime.Interpreter (interpret, evaluate) where

import qualified Retime.Parser as Ast (Ast(..))
import Retime.Interpreter.Types
import Data.IORef

interpret :: Ast.Ast -> (IORef Symtab) -> (IORef ArgState) -> Bool -> IO Symbol
evaluate :: Symbol -> [Symbol] -> (IORef Symtab) -> IO Symbol
