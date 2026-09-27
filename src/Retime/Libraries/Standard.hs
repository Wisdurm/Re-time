module Retime.Libraries.Standard where

import Retime.Interpreter.Types
import Retime.Interpreter.Convert
import Retime.Interpreter
import qualified Data.Text as T
import Control.Monad
import Data.IORef

-- | BUILTIN: Does nothing
bNil :: Object
bNil = BuiltIn $ \args symRef -> do
  emptyObject

-- | BUILTIN: Prints all arguments
bPrint :: Object
bPrint = BuiltIn $ \args symRef -> do
  a <- forM args debugP
  forM_ a putStr
  putStrLn ""
  emptyObject

-- | BUILTIN: Prints all arguments (evaluated)
bLog :: Object
bLog = BuiltIn $ \args symRef -> do
  xs <- forM args (\s -> evaluate s [] symRef)
  a <- forM xs debugP
  forM_ a putStr
  putStrLn ""
  emptyObject

-- | BUILTIN: Prints all arguments as strings
bPut :: Object
bPut = BuiltIn $ \args symRef -> do
  a <- forM args (getText symRef)
  forM_ a (putStr . T.unpack)
  putStrLn ""
  emptyObject

-- | BUILTIN: Gets user input
bInput :: Object
bInput = BuiltIn $ \args symRef -> do
  line <- getLine
  textObject . T.pack $ line

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
  newIORef (Left . Object $ args)

-- | BUILTIN: Conditional evaluation
bIf :: Object
bIf = BuiltIn $ \args symRef -> do
  let ifo = args !! 1
      elo = args !! 2
  cond <- getBool symRef (head args)
  if cond then evaluate ifo [] symRef
    else evaluate elo [] symRef

-- | BUILTIN: Invert boolean
bNot :: Object
bNot = BuiltIn $ \args symRef -> do
  cond <- getBool symRef (head args)
  if cond then emptyObject
    else do
    newIORef . Right $ 1

-- | Return the first member of an object
bHead :: Object
bHead = BuiltIn $ \args symRef -> do
  let sym = head args
  e <- readIORef sym
  case e of
    Right v -> return sym
    Left obj -> case obj of
      Object [] -> emptyObject
      Object members -> return . head $ members
      BuiltIn _ -> error "Cannot get head of builtin"
      Thunk ast -> do
        v <- evaluate sym [] symRef
        let (BuiltIn f) = bHead
        f [v] symRef

-- | Return everything but the first member of an object
bTail :: Object
bTail = BuiltIn $ \args symRef -> do
  let sym = head args
  e <- readIORef sym
  case e of
    Right v -> emptyObject
    Left obj -> case obj of
      Object [] -> emptyObject
      Object members -> do
        t <- newIORef (Left . Object . tail $ members)
        return t
      BuiltIn _ -> error "Cannot get tail of builtin"
      Thunk ast -> do
        v <- evaluate sym [] symRef
        let (BuiltIn f) = bTail
        f [v] symRef

-- | Append tail to head
bList :: Object
bList = BuiltIn $ \args symRef -> do
  let h = args !! 0
      t = args !! 1
  -- Get members from tail
  e <- readIORef t
  xs <- case e of
    Right v -> return [t]
    Left obj -> case obj of
      Object [] -> return []
      Object members -> do
        return members
      BuiltIn _ -> error "Cannot get tail of builtin"
      Thunk ast -> error "not yet"

  newIORef (Left . Object $ (h:xs))

-- | Returns true if object is empty
bEmpty :: Object
bEmpty = BuiltIn $ \args symRef -> do
  e <- readIORef . head $ args
  case e of
    Right v -> emptyObject
    Left obj -> case obj of
      Object [] -> valueOne
      Object members -> emptyObject
      BuiltIn _ -> error "Cannot get tail of builtin"
      Thunk ast -> error "not yet"

-- | Repeat body while condition is true
bWhile :: Object
bWhile = BuiltIn $ \args symRef -> do
  cond <- getBool symRef (head args)
  let body = args !! 1
  if cond then do
    evaluate body [] symRef
    let (BuiltIn f) = bWhile
    f args symRef
    else emptyObject

-- | BUILTIN: Convert array interpreted as string into number
bRead :: Object
bRead = BuiltIn $ \args symRef -> do
  str <- getText symRef (head args)
  let v = read (T.unpack str) :: Double
  newIORef (Right v)
