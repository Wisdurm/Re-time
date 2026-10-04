module Retime.Shell.Types where

import Retime.Interpreter.Types (Symtab())
import Foreign.StablePtr
import Data.IORef

type RetimeState = StablePtr (IORef Symtab)
