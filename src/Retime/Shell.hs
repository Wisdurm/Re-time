{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
module Retime.Shell where

import Retime (interpretTextWithSym)
import Retime.Interpreter (lookupSymtab)
import Retime.Interpreter.Types
import Retime.Interpreter.Convert (getNumber)
import Retime.Libraries (defaultSymtab)
import Retime.Shell.Types
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import Data.IORef
import Foreign.C.Types
import Foreign.C.String
import Foreign.StablePtr

-- | Test Retime
testRetime :: IO ()
testRetime = putStrLn "Retime hello!"

-- | Creates a new Retime state, which stores objects and can be
-- used when calling Retime code. You must destroy the state after
-- you no longer need it with destoryRetimeState
createRetimeState :: IO RetimeState
createRetimeState = do
  symRef <- defaultSymtab
  ptr <- newStablePtr symRef
  return ptr

-- | Destroys a Retime state.
destroyRetimeState :: RetimeState -> IO ()
destroyRetimeState rst = do
  freeStablePtr rst

-- | Prints a message which verifies the state is valid
validateRetimeState :: RetimeState -> IO ()
validateRetimeState rst = do
  symRef <- deRefStablePtr rst
  st <- readIORef symRef
  let (Symtab m _ _) = st
  if HM.member (T.pack "Nil") m
    then putStrLn "Everything fine!"
    else putStrLn "Everything broken!"

-- | Get a double value from a Retime symbol
readDoubleRetimeState :: RetimeState -> CString -> IO CDouble
readDoubleRetimeState rst cstr = do
  name <- peekCString cstr
  symRef <- deRefStablePtr rst
  argState <- defaultArgState
  sym <- lookupSymtab (T.pack name) symRef argState
  val <- getNumber symRef sym
  print val
  return (CDouble val)

-- | Run code in a Retime state
runInRetimeState :: RetimeState -> CString -> IO ()
runInRetimeState rst cstr = do
  code <- peekCString cstr
  symRef <- deRefStablePtr rst
  _ <- interpretTextWithSym symRef (T.pack code)
  return ()

foreign export capi testRetime :: IO ()
foreign export capi createRetimeState :: IO RetimeState
foreign export capi destroyRetimeState :: RetimeState -> IO ()
foreign export capi readDoubleRetimeState :: RetimeState -> CString -> IO CDouble
foreign export capi validateRetimeState :: RetimeState -> IO ()
foreign export capi runInRetimeState :: RetimeState -> CString -> IO ()
