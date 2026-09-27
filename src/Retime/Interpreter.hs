module Retime.Interpreter (interpret, evaluate, getNumber,
                           debugP, defaultArgState) where

import qualified Retime.Parser as Ast (Ast(..))
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import Retime.Interpreter.Types
import GHC.StableName
import Control.Monad
import Data.IORef
import Data.Char (ord)

-- | Interprets an ast node in a certain context.
-- If the last arg is false, leave thunks, otherwise evaluate.
interpret :: Ast.Ast -> (IORef Symtab) -> (IORef ArgState) -> Bool -> IO Symbol
interpret (Ast.Call (Ast.Identifier name) args) symRef argRef True = do
  fun <- lookupSymtab name symRef argRef
  evalArgs <- forM args (\node -> interpret node symRef argRef False)
  -- Go down in scope
  sym <- readIORef symRef
  ctx <- makeStableName fun
  nSymRef <- newIORef (Symtab HM.empty (Just sym) ctx)
  evaluate fun evalArgs nSymRef
interpret (Ast.Call name args) _ _ False = do
  objRef <- newIORef (Left $ Thunk (Ast.Call name args))
  return objRef
interpret (Ast.Identifier name) symRef argRef _ = do
  lookupSymtab name symRef argRef
interpret (Ast.Literal (Right num)) _ _ _ = do
  valRef <- newIORef (Right num)
  return valRef
interpret (Ast.Literal (Left str)) _ _ _ = do
  chars <- forM (T.unpack str) (\c -> newIORef (Right . fromIntegral . ord $ c))
  strRef <- newIORef (Left . Object $ chars)
  return strRef

-- | Evaluate an object, with possible side-effects
evaluate :: Symbol -> [Symbol] -> (IORef Symtab) -> IO Symbol
evaluate sRef args symRef = do
  sym <- readIORef sRef
  case sym of
    Left obj ->
      case obj of
        Object [] -> do
          ref <- emptyObject
          return ref
        Object symbols -> do
          mapM_ (\o -> evaluate o args symRef) (init symbols)
          evaluate (last symbols) args symRef
        BuiltIn fun -> do
          fun args symRef
        Thunk ast -> do
          argRef <- newIORef args
          interpret ast symRef argRef True
    _ -> return sRef

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
          s <- interpret ast symRef st True
          getNumber symRef s

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

-- | Modifies the members of a symbol table with a function
modifyMembers :: (HM.HashMap T.Text Symbol -> HM.HashMap T.Text Symbol) ->
                 Symtab -> Symtab
modifyMembers f (Symtab m p c) = Symtab (f m) p c

-- | Looks up a value from a symbol table and it's parents.
-- If nothing is found, creates a new empty value, which uses
-- any possible arguments from the current arg state.
-- If something is found, but it was created in a different call of
-- the same function, ignore it.
lookupSymtab :: T.Text -> (IORef Symtab) -> (IORef ArgState) -> IO Symbol
lookupSymtab k symRef argRef = do
  sym <- readIORef symRef
  -- TODO: rootContext is a horrible name but I can't think of anything better
  let rootContext = context sym
  case lookup' k sym 0 rootContext of
    Just v -> return v
    Nothing -> do
      symbol <- popArgument argRef
      modifyIORef symRef (\sym -> modifyMembers (HM.insert k symbol) sym)
      return symbol
  where
    lookup' :: T.Text -> Symtab -> Int -> StableName Symbol -> Maybe Symbol
    lookup' k (Symtab m parent ctx) depth root =
      case HM.lookup k m of
        Just v -> if ctx == root && depth > 0 then Nothing
                  else Just v
        Nothing -> case parent of
                     Nothing -> Nothing
                     Just p -> lookup' k p (depth+1) root

-- | Creates an empty argstate
defaultArgState :: IO (IORef ArgState)
defaultArgState = newIORef []

-- | Pops an argument of the (bottom of the) argstate
popArgument :: IORef ArgState -> IO Symbol
popArgument argRef = do
  argState <- readIORef argRef
  case argState of
    [] -> do
      x <- emptyObject
      return x
    -- Pop first arg
    (x:xs) -> do
      writeIORef argRef xs
      return x
