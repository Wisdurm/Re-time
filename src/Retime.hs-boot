module Retime where

import qualified Data.Text as T
import Retime.Interpreter.Types as Intp
import Data.IORef

interpretTextWithSym' :: IORef Symtab -> T.Text -> IO String
interpretTextWithSym :: IORef Symtab -> T.Text -> IO String
