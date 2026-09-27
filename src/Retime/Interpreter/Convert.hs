module Retime.Interpreter.Convert (getNumber, getText, getBool,
                                   debugP, textObject) where

import {-# SOURCE#-} Retime.Interpreter
import Retime.Interpreter.Types
import qualified Data.Text as T
import Control.Monad
import Data.IORef
import Data.Char (ord, chr)

-- | Gets a number value out of an object.
-- Empty object = 0. Does evaluate when necessary.
getNumber :: (IORef Symtab) -> Symbol -> IO Double
getNumber symRef ref = do
  sym <- readIORef ref
  case sym of
    (Right val) -> return val
    (Left obj) ->
      case obj of
        Object [] -> return 0
        Object symbols -> do
          getNumber symRef (last symbols)
        BuiltIn _ -> error "blud"
        Thunk ast -> do
          st <- defaultArgState
          sy <- interpret ast symRef st True
          getNumber symRef sy

-- | Gets a string value out of an object.
-- Empty object = "". Does evaluate when necessary.
getText :: (IORef Symtab) -> Symbol -> IO T.Text
getText symRef ref = do
  sym <- readIORef ref
  case sym of
    (Right val) -> return . T.pack . show $ val
    (Left obj) ->
      case obj of
        Object [] -> return . T.pack $ ""
        Object symbols -> do
          nums <- mapM (getNumber symRef) symbols
          return . T.pack $ (map (chr . round) nums)
        BuiltIn _ -> error "blud"
        Thunk ast -> do
          st <- defaultArgState
          sy <- interpret ast symRef st True
          getText symRef sy

-- | Gets a bool value out of an object.
-- Empty object = false. Always evaluates
getBool :: (IORef Symtab) -> Symbol -> IO Bool
getBool symRef ref = do
  cond' <- evaluate ref [] symRef
  cond <- readIORef cond'
  case cond of
    Left (Object []) -> return False
    _ -> return True

-- | Turns a string into a Retime object
textObject :: T.Text -> IO Symbol
textObject str = do
  chars <- forM (T.unpack str) (\c -> newIORef (Right . fromIntegral . ord $ c))
  newIORef (Left . Object $ chars)

-- | Debug print a symbol
debugP :: Symbol -> IO String
debugP sRef = do
  sym <- readIORef sRef
  case sym of
    (Right val) -> return . show $ val
    (Left obj) ->
      case obj of
        Object symbols -> do
          tree <- mapM debugP symbols
          return ("[" ++ unwords tree ++ "]")
        BuiltIn _ -> do
          return "Builtin"
        Thunk ast -> return ("THUNK|"++(show ast)++"|THUNK")
