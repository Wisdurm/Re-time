{-# LANGUAGE OverloadedStrings #-}
module Retime.Libraries (defaultSymtab) where

import Retime.Libraries.Standard
import Retime.Libraries.Math
import Retime.Interpreter.Types
import qualified Data.HashMap.Lazy as HM
import GHC.StableName
import Data.IORef

-- | Creates an empty symbol table with no parent
defaultSymtab :: IO (IORef Symtab)
defaultSymtab = do
  mainScope <- emptyObject
  mainContext <- makeStableName mainScope
  let pairs = [("Nil", bNil),
               ("Print", bPrint),
                ("Log", bLog),
                ("Put", bPut),
                ("Input", bInput),
                ("Series", bSeries),
                ("Convert", bConvert),
                ("Copy", bCopy),
                ("Set", bSet),
                ("Object", bObject),
                ("If", bIf),
                ("Not", bNot),
                ("While", bWhile),
                ("+", bAdd),
                ("-", bNegate),
                ("=", bCompare),
                ("*", bMultiply),
                ("<", bSmaller),
                ("Head", bHead),
                ("Tail", bTail),
                ("List", bList),
                ("Read", bRead),
                ("Empty", bEmpty),
                ("Exit", bExit)
                ]
  funcs <- mapM newIORef (map Left (snd . unzip $ pairs))
  let npairs = zip (fst . unzip $ pairs) funcs
  newIORef (Symtab (HM.fromList npairs) Nothing mainContext)
