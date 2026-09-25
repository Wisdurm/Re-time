module Retime.Libraries where

import Retime.Libraries.Standard
import Retime.Interpreter.Types
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import GHC.StableName
import Data.IORef

-- | Creates an empty symbol table with no parent
defaultSymtab :: IO (IORef Symtab)
defaultSymtab = do
  -- TODO: Better
  mainScope <- emptyObject
  mainContext <- makeStableName mainScope
  -- TODO: Better everything
  nil <- emptyObject
  p <- newIORef . Left $ bPrint
  s <- newIORef . Left $ bSeries
  c <- newIORef . Left $ bConvert
  cc <- newIORef . Left $ bCopy
  o <- newIORef . Left $ bObject
  ss <- newIORef . Left $ bSet
  i <-  newIORef . Left $ bIf
  pl <- newIORef . Left $ bPlus
  m <- newIORef . Left $ bNeg
  co <- newIORef . Left $ bComp
  t <- newIORef . Left $ bTim
  l <- newIORef . Left $ bLog
  h <- newIORef . Left $ bHead
  tt <- newIORef . Left $ bTail
  ll <- newIORef . Left $ bList
  e <- newIORef . Left $ bEmpty

  newIORef (Symtab (HM.fromList [(T.pack "Print", p),
                                  (T.pack "Log", l),
                                 (T.pack "Series", s),
                                  (T.pack "Convert", c),
                                  (T.pack "Copy", cc),
                                  (T.pack "Set", ss),
                                  (T.pack "Object", o),
                                  (T.pack "If", i),
                                  (T.pack "Add", pl),
                                  (T.pack "Minus", m),
                                  (T.pack "Comp", co),
                                  (T.pack "Mult", t),
                                  (T.pack "Head", h),
                                  (T.pack "Tail", tt),
                                  (T.pack "List", ll),
                                  (T.pack "Nil", nil),
                                  (T.pack "Empty", e)
                                ]) Nothing mainContext)
