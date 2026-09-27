{-# LANGUAGE OverloadedStrings #-}
module Retime.Libraries where

import Retime.Libraries.Standard
import Retime.Interpreter.Types
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import GHC.StableName
import Control.Monad
import Data.IORef

-- | Creates an empty symbol table with no parent
defaultSymtab :: IO (IORef Symtab)
defaultSymtab = do
  mainScope <- emptyObject
  mainContext <- makeStableName mainScope
  let pairs = [("Nil", bNil),
               ("Print", bPrint),
               ("Log", bLog),
               ("Series", bSeries),
               ("Convert", bConvert),
               ("Copy", bCopy),
               ("Set", bSet),
               ("Object", bObject),
               ("If", bIf),
               ("+", bAdd),
               ("-", bNegate),
               ("=", bCompare),
               ("*", bMultiply),
               ("Head", bHead),
               ("Tail", bTail),
               ("List", bList),
               ("Empty", bEmpty)]
  funcs <- mapM newIORef (map Left (snd . unzip $ pairs))
  let npairs = zip (fst . unzip $ pairs) funcs
  newIORef (Symtab (HM.fromList npairs) Nothing mainContext)
