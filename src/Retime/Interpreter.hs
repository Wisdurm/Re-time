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

-- | Represents the current calling args at any point in time
type ArgState = [Symbol]

type Symbol = Either (IORef Object) (IORef Double)

data Object = Object [Symbol]
            | Thunk Ast.Ast
            | BuiltIn ([Symbol] -> (IORef Symtab) -> (IORef ArgState) -> IO Symbol)

-- | Helper which creates an empty object
emptyObject :: IO (IORef Object)
emptyObject = do
      ref <- newIORef (Object [])
      return ref

-- | Interprets an ast node in a certain context.
-- If the last arg is false, leave thunks, otherwise evaluate
-- TODO: Arg state
interpret :: Ast.Ast -> (IORef Symtab) -> (IORef ArgState) -> Bool -> IO Symbol
interpret (Ast.Call (Ast.Identifier name) args) symRef argRef True = do
  fun <- lookupSymtab name symRef argRef
  evalArgs <- forM args (\node -> interpret node symRef argRef False)
  evaluate fun evalArgs symRef
interpret (Ast.Call name args) symRef _ False = do
  objRef <- newIORef (Thunk (Ast.Call name args))
  return (Left objRef)
interpret (Ast.Identifier name) symRef argRef _ = do
  lookupSymtab name symRef argRef
interpret (Ast.Literal (Right num)) _ _ _ = do
  valRef <- newIORef num
  return (Right valRef)
interpret (Ast.Literal (Left str)) _ _ _ = do
  chars <- forM (T.unpack str) (\c -> newIORef (fromIntegral . ord $ c))
  strRef <- newIORef (Object (map Right chars))
  return (Left strRef)

-- | Evaluate an object, with possible side-effects
evaluate :: Symbol -> [Symbol] -> (IORef Symtab) -> IO Symbol
evaluate (Left objRef) args symRef = do
  obj <- readIORef objRef
  case obj of
    Object [] -> do
      ref <- newIORef (Object [])
      return (Left ref)
    Object symbols -> do
      mapM_ (\o -> evaluate o [] symRef) (init symbols)
      evaluate (last symbols) [] symRef
    BuiltIn fun -> do
      argRef <- newIORef (args)
      fun args symRef argRef
    Thunk ast -> do
      argRef <- newIORef (args)
      interpret ast symRef argRef True
evaluate val args symRef = return val

-- | Debug print a symbol
debugP :: Symbol -> IO String
debugP (Right ref) = do
  ref <- readIORef $ ref
  return . show $ ref
debugP (Left obj) = do
  object <- readIORef obj
  case object of
    Object symbols -> do
      tree <- mapM debugP symbols
      return ("[" ++ unwords tree ++ "]")
    BuiltIn _ -> do
      return "Builtin"
    Thunk _ -> error "Thunk was not evaluated"

-- | Creates an empty symbol table with no parent
defaultSymtab :: IO (IORef Symtab)
defaultSymtab = do
  p <- newIORef bPrint
  s <- newIORef bSeries
  ss <- newIORef bSet
  newIORef (Symtab (HM.fromList [(T.pack "Print", Left p),
                                 (T.pack "Series", Left s),
                                  (T.pack "Set", Left ss)
                                ]) Nothing)

-- | Modifies the members of a symbol table with a function
modifyMembers :: (HM.HashMap T.Text Symbol -> HM.HashMap T.Text Symbol) ->
                 Symtab -> Symtab
modifyMembers f (Symtab m p) = Symtab (f m) p

-- | Looks up a value from a symbol table and it's parents.
-- If nothing is found, creates a new empty value, which uses
-- any possible arguments from the current arg state
lookupSymtab :: T.Text -> (IORef Symtab) -> (IORef ArgState) -> IO Symbol
lookupSymtab k symRef argRef = do
  sym <- readIORef symRef
  case lookup' k sym of
    Just v -> return v
    Nothing -> do
      argState <- readIORef argRef
      symbol <- case argState of
                  [] -> do
                    x <- newIORef $ (Object [])
                    return . Left $ x
                  -- Pop first arg
                  (x:xs) -> do
                    writeIORef argRef xs
                    return x
      modifyIORef symRef (\sym -> modifyMembers (HM.insert k symbol) sym)
      return symbol

-- | Recursively looks up a key from a symbol table
lookup' :: T.Text -> Symtab -> Maybe Symbol
lookup' k (Symtab m parent) =
  case HM.lookup k m of
    Just v -> Just v
    Nothing -> case parent of
                 Nothing -> Nothing
                 Just p -> lookup' k p

-- | BUILTIN: Prints all arguments
bPrint :: Object
bPrint = BuiltIn $ \args symRef argRef -> do
  a <- forM args debugP
  forM_ a print
  o <- emptyObject
  return . Left $ o
-- | BUILTIN: Evaluates all arguments
bSeries :: Object
bSeries = BuiltIn $ \args symRef argRef -> do
  mapM_ (\o -> evaluate o [] symRef) (init args)
  evaluate (last args) [] symRef
-- | BUILTIN: Sets the values of an object, overriding
bSet :: Object
bSet = BuiltIn $ \args symRef argRef -> do
  case take 1 args of
    [] -> error "No args todo: return []"
    [(Right _)] -> error "later"
    [(Left objRef)] -> do
      obj <- readIORef objRef
      case obj of
        Object _ -> do
          let members = drop 1 args
          writeIORef objRef (Object members)
          return (Left objRef)
