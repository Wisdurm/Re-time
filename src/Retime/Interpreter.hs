module Retime.Interpreter (interpret, debugP, defaultSymtab) where

import qualified Retime.Parser as Ast (Ast(..))
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import Control.Monad
import Data.IORef
import Data.Char (ord)

data Symtab = Symtab {
  members :: HM.HashMap T.Text Symbol,
  parent :: Maybe Symtab
                     }

type Symbol = Either (IORef Object) (IORef Double)

data Object = Object [Symbol]
            | Thunk Ast.Ast

interpret :: Ast.Ast -> (IORef Symtab) -> IO Symbol
interpret (Ast.Identifier name) symRef = do
  sym <- readIORef symRef
  case HM.lookup name (members sym) of
    Just v -> return v
    Nothing -> do
      ref <- newIORef (0 :: Double)
      modifyIORef symRef (\sym -> modifyMembers (HM.insert name (Right ref)) sym)
      return (Right ref)
interpret (Ast.Literal (Right num)) _ = do
  ref <- newIORef num
  return (Right ref)
interpret (Ast.Literal (Left str)) _ = do
  chars <- forM (T.unpack str) (\c -> newIORef (fromIntegral . ord $ c))
  ref <- newIORef (Object (map Right chars))
  return (Left ref)

defaultSymtab :: IO (IORef Symtab)
defaultSymtab = newIORef (Symtab HM.empty Nothing)

modifyMembers :: (HM.HashMap T.Text Symbol -> HM.HashMap T.Text Symbol) ->
                 Symtab -> Symtab
modifyMembers f (Symtab m p) = Symtab (f m) p

-- Debug print
debugP :: Symbol -> IO String
debugP (Right ref) = do
  ref <- readIORef $ ref
  return . show $ ref
debugP (Left obj) = do
  object <- readIORef obj
  let (Object symbols) = object
  tree <- mapM debugP symbols
  return ("[" ++ unwords tree ++ "]")
