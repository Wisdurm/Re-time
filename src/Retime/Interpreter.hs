module Retime.Interpreter (interpret, debugP, defaultSymtab,
                           defaultArgState) where

import qualified Retime.Parser as Ast (Ast(..))
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import GHC.StableName
import Control.Monad
import Data.IORef
import Data.Char (ord)

data Symtab = Symtab {
  --  members :: HM.HashMap T.Text (HM.HashMap (StableName Symbol) Symbol),
  -- OR store Symtab reference somewhere?
  -- just need to compare Symboltables or something?
  members :: HM.HashMap T.Text Symbol,
  parent :: Maybe Symtab,
  context :: StableName Symbol
                     }

-- | Represents the current calling args at any point in time
type ArgState = [Symbol]

type Symbol = IORef (Either Object Double)

data Object = Object [Symbol]
            | Thunk Ast.Ast
            | BuiltIn ([Symbol] -> (IORef Symtab) -> IO Symbol)

-- | Helper which creates an empty object
emptyObject :: IO Symbol
emptyObject = do
      ref <- newIORef (Left . Object $ [])
      return ref

-- | Interprets an ast node in a certain context.
-- If the last arg is false, leave thunks, otherwise evaluate
-- TODO: Arg state
interpret :: Ast.Ast -> (IORef Symtab) -> (IORef ArgState) -> Bool -> IO Symbol
interpret (Ast.Call (Ast.Identifier name) args) symRef argRef True = do
  fun <- lookupSymtab name symRef argRef
  evalArgs <- forM args (\node -> interpret node symRef argRef False)
  -- Go down in scope
  sym <- readIORef symRef
  ctx <- makeStableName fun
  nSymRef <- newIORef (Symtab HM.empty (Just sym) ctx)
  evaluate fun evalArgs nSymRef
interpret (Ast.Call name args) symRef _ False = do
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
    (Left obj) ->
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
    val -> return sRef

-- | Gets a number value out of an object.
-- Empty object = 0. Does not evaluate.
getNumber :: Symbol -> IO Double
getNumber ref = do
  sym <- readIORef ref
  case sym of
    (Right val) -> return val
    (Left obj) ->
      case obj of
        Object [] -> return 0
        Object symbols -> do
          getNumber (last symbols)
        BuiltIn _ -> error "blud"
        Thunk _ -> return 1234

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
        Thunk _ -> return "Thunk" -- ("THUNK|"++(show ast)++"|THUNK")

-- | Creates an empty symbol table with no parent
defaultSymtab :: IO (IORef Symtab)
defaultSymtab = do
  -- TODO: Better
  mainScope <- emptyObject
  mainContext <- makeStableName mainScope
  -- TODO: Better everything
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

  f' <- newIORef . Left . Thunk $ (Ast.Call (Ast.Identifier (T.pack "Print"))
                                   [Ast.Identifier (T.pack "arg" )])
  f <- newIORef . Left . Object $ [f']

  g' <- newIORef . Left . Thunk $ (Ast.Call (Ast.Identifier (T.pack "Log"))
                                   [Ast.Identifier (T.pack "arg" )])
  g'' <- newIORef . Left . Thunk $ (Ast.Call (Ast.Identifier (T.pack "g"))
                                    [Ast.Call (Ast.Identifier (T.pack "Add"))
                                     [Ast.Identifier (T.pack "arg"),
                                      Ast.Literal (Right 1)]])
  g <- newIORef . Left . Object $ [g', g'']
  newIORef (Symtab (HM.fromList [(T.pack "Print", p),
                                  (T.pack "Log", l),
                                 (T.pack "Series", s),
                                  (T.pack "Convert", c),
                                  (T.pack "Copy", cc),
                                  (T.pack "Set", ss),
                                  (T.pack "Object", o),
                                  (T.pack "f", f),
                                  (T.pack "g", g),
                                  (T.pack "If", i),
                                  (T.pack "Add", pl),
                                  (T.pack "Minus", m),
                                  (T.pack "Comp", co),
                                  (T.pack "Mult", t)
                                ]) Nothing mainContext)

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
defaultArgState = do
  ref <- newIORef []
  return ref

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

-- | BUILTIN: Prints all arguments
bPrint :: Object
bPrint = BuiltIn $ \args symRef -> do
  a <- forM args debugP
  forM_ a print
  o <- emptyObject
  return o
-- | BUILTIN: Prints all arguments (evaluated)
bLog :: Object
bLog = BuiltIn $ \args symRef -> do
  xs <- forM args (\s -> evaluate s [] symRef)
  a <- forM xs debugP
  forM_ a print
  o <- emptyObject
  return o
-- | BUILTIN: Evaluates all arguments
bSeries :: Object
bSeries = BuiltIn $ \args symRef -> do
  mapM_ (\o -> evaluate o [] symRef) (init args)
  evaluate (last args) [] symRef
-- | BUILTIN: Sets the values of an object, overriding
bConvert :: Object
bConvert = BuiltIn $ \args symRef -> do
  case take 1 args of
    [] -> error "No args todo: return []"
    [sRef] -> do
      let members = drop 1 args
      writeIORef sRef (Left . Object $ members)
      return sRef
-- | BUILTIN: Copies the value of a symbol into another, overriding
bCopy :: Object
bCopy = BuiltIn $ \args symRef -> do
  case take 1 args of
    [] -> error "No args todo: return []"
    [sRef] -> do
      val <- readIORef (args !! 1)
      writeIORef sRef val
      return sRef
-- | BUILTIN: Same as Copy, but evaluates argument
bSet :: Object
bSet = BuiltIn $ \args symRef -> do
  case take 1 args of
    [] -> error "No args todo: return []"
    [sRef] -> do
      r <- evaluate (args !! 1) [] symRef
      val <- readIORef r
      writeIORef sRef val
      return sRef
-- | BUILTIN: Creates an object with members
bObject :: Object
bObject = BuiltIn $ \args symRef -> do
  o <- newIORef (Left . Object $ args)
  return o
-- | BUILTIN: Conditional evaluation
bIf :: Object
bIf = BuiltIn $ \args symRef -> do
  let ifo = args !! 1
      elo = args !! 2
  cond' <- evaluate (head args) [] symRef
  cond <- readIORef cond'
  case cond of
    Left (Object []) -> evaluate elo [] symRef
    _ -> evaluate ifo [] symRef
-- | Add all arguments
bPlus :: Object
bPlus = BuiltIn $ \args symRef -> do
  nums <- mapM getNumber args
  val <- newIORef (Right . sum $ nums)
  return val
-- | Negate all arguments
bNeg :: Object
bNeg = BuiltIn $ \args symRef -> do
  nums <- mapM getNumber args
  val <- newIORef (Right (head nums - (sum . tail $ nums)))
  return val
-- | Multiply all arguments
bTim :: Object
bTim = BuiltIn $ \args symRef -> do
  nums <- mapM getNumber args
  -- Could do with monoids but dont want import for 1 line...
  val <- newIORef (Right (foldr (\x y -> x * y) 1 nums))
  return val
-- | Compare arguments as numbers
bComp :: Object
bComp = BuiltIn $ \args symRef -> do
  nums <- mapM getNumber args
  if allSame nums then
    return (head args)
    else do
    o <- emptyObject
    return o
    where allSame [] = True -- NOT SUPER EFFICIENT BUT GOOD FOR NOW
          allSame (_:[]) = True
          allSame (x:y:xs)
            | x == y = allSame (y:xs)
            | otherwise = False
