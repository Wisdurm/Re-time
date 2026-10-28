module Retime.Interpreter.Types where

import qualified Retime.Parser as Ast (Ast(..))
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import GHC.StableName
import Data.IORef

import Control.Monad

-- | A symbol table maps identifiers to symbols
data Symtab = Symtab { members :: HM.HashMap T.Text Symbol
                     , parent :: Maybe Symtab
                     , context :: StableName Symbol
                     }

-- | Represents the current calling args at any point in time
type ArgState = [Symbol]

-- | A symbol is either an object or value
type Symbol = IORef (Either Object Double)

-- | An object is opaquely supposed to just be a list of members.
-- Internally it can also be a thunk, or a built-in function.
data Object = Object [Symbol]
            | Thunk Ast.Ast
            | BuiltIn ([Symbol] -> (IORef Symtab) -> IO Symbol)

-- | Helper which creates an empty object
emptyObject :: IO Symbol
emptyObject = do
      ref <- newIORef (Left . Object $ [])
      return ref

-- | Helper which creates a value of 1
valueOne :: IO Symbol
valueOne = do
      ref <- newIORef (Right 1)
      return ref

-- | Helper which creates a value of 0
valueZero :: IO Symbol
valueZero = do
      ref <- newIORef (Right 0)
      return ref

-- | Creates an empty argstate
defaultArgState :: IO (IORef ArgState)
defaultArgState = newIORef []

-- | Debug function
printSymtab :: IORef Symtab -> IO ()
printSymtab symRef = do
  st <- readIORef symRef
  putStrLn "Symtab:"
  printSymtab' st

printSymtab' :: Symtab -> IO ()
printSymtab' st = do
  let (Symtab members p _) = st
  forM_ (HM.keys members) print
  -- case p of
  --   Just pst -> printSymtab' pst
  --   Nothing -> return ()
