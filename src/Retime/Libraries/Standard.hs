{-# LANGUAGE OverloadedStrings #-}
module Retime.Libraries.Standard where

import Retime.Interpreter.Types
import Retime.Interpreter.Convert
import Retime.Interpreter
import qualified Data.Text as T
import Control.Monad
import Data.IORef
import System.Exit
import Text.Read (readMaybe)

-- | BUILTIN: Does nothing
bNil :: Object
bNil = BuiltIn $ \_ _ -> do
  emptyObject

-- | BUILTIN: Breaks everything
bKill :: Object
bKill = undefined

-- | BUILTIN: Prints all arguments
bPrint :: Object
bPrint = BuiltIn $ \args _ -> do
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
bInput = BuiltIn $ \_ _ -> do
  line <- getLine
  textObject . T.pack $ line

-- | BUILTIN: Evaluates all arguments
bSeries :: Object
bSeries = BuiltIn $ \args symRef -> do
  if null args then emptyObject
    else do mapM_ (\o -> evaluate o [] symRef) (init args)
	    evaluate (last args) [] symRef

-- | BUILTIN: Sets the values of an object, overriding
bConvert :: Object
bConvert = BuiltIn $ \args _ -> do
  case take 1 args of
    [] -> emptyObject
    (sRef:_) -> do
      let members = drop 1 args
      writeIORef sRef (Left . Object $ members)
      return sRef

-- | BUILTIN: Copies the value of a symbol into another, overriding
bCopy :: Object
bCopy = BuiltIn $ \args _ -> do
  case take 1 args of
    [] -> emptyObject
    (sRef:_) -> do
      if length args == 1
        then return sRef
        else do
        val <- readIORef (args !! 1)
        writeIORef sRef val
        return sRef

-- | BUILTIN: Same as Copy, but evaluates argument
bSet :: Object
bSet = BuiltIn $ \args symRef -> do
  case take 1 args of
    [] -> emptyObject
    (sRef:_) -> do
       if length args == 1
        then return sRef
        else do
         r <- evaluate (args !! 1) [] symRef
         val <- readIORef r
         writeIORef sRef val
         return sRef

-- | BUILTIN: Creates an object with members
bObject :: Object
bObject = BuiltIn $ \args _ -> do
  newIORef (Left . Object $ args)

-- | BUILTIN: Conditional evaluation
bIf :: Object
bIf = BuiltIn $ \args symRef -> do
  case take 1 args of
    [] -> emptyObject
    (condRef:_) -> do
      cond <- getBool symRef condRef
      if cond
        then if length args < 2 then emptyObject
             else evaluate (args !! 1) [] symRef
        else if length args < 3 then emptyObject
             else evaluate (args !! 2) [] symRef

-- | BUILTIN: Invert boolean
bNot :: Object
bNot = BuiltIn $ \args symRef -> do
  case take 1 args of
    [] -> valueOne -- HAHAHAHHAHAHH
    (condRef:_) -> do
      cond <- getBool symRef condRef
      if cond then emptyObject
        else valueOne

-- | Return the first member of an object
bHead :: Object
bHead = BuiltIn $ \args symRef -> do
  case take 1 args of
    [] -> emptyObject
    (sym:_) -> do
      e <- readIORef sym
      case e of
        Right _ -> return sym
        Left obj -> case obj of
          Object [] -> emptyObject
          Object members -> return . head $ members
          BuiltIn _ -> error "Cannot get head of builtin"
          Thunk _ -> do
            v <- evaluate sym [] symRef
            let (BuiltIn f) = bHead
            f [v] symRef

-- | Return everything but the first member of an object
bTail :: Object
bTail = BuiltIn $ \args symRef -> do
  case take 1 args of
    [] -> emptyObject
    (sym:_) -> do
      e <- readIORef sym
      case e of
        Right _ -> emptyObject
        Left obj -> case obj of
          Object [] -> emptyObject
          Object members -> do
            t <- newIORef (Left . Object . tail $ members)
            return t
          BuiltIn _ -> error "Cannot get tail of builtin"
          Thunk _ -> do
            v <- evaluate sym [] symRef
            let (BuiltIn f) = bTail
            f [v] symRef

-- | Append tail to head
bList :: Object
bList = BuiltIn $ \args symRef -> do
  case take 1 args of
    [] -> emptyObject
    (h:_) -> if length args < 2 then return h
      else do
      let t = args !! 1
      -- Get members from tail
      e <- readIORef t
      xs <- case e of
        Right _ -> return [t]
        Left obj -> case obj of
                      Object [] -> return []
                      Object members -> do
                        return members
                      BuiltIn _ -> error "Cannot get tail of builtin"
                      Thunk _ -> error "fuggg"
      newIORef (Left . Object $ (h:xs))

-- | Repeat body while condition is true
bWhile :: Object
bWhile = BuiltIn $ \args symRef -> do
  case take 1 args of
    [] -> emptyObject
    (condRef:_) -> do
      cond <- getBool symRef condRef
      if cond then if length args < 2 then undefined
        else do
        let body = args !! 1
        _ <- evaluate body [] symRef
        let (BuiltIn f) = bWhile
        f args symRef
        else emptyObject

-- | BUILTIN: Convert array interpreted as string into number
bRead :: Object
bRead = BuiltIn $ \args symRef -> do
  str <- getText symRef (head args)
  case readMaybe (T.unpack . T.replace "," "." $ str) :: Maybe Double of
    Just v -> newIORef (Right v)
    _ -> emptyObject

-- | BUILTIN: Exit program with code
bExit :: Object
bExit = BuiltIn $ \args symRef -> do
  code <- if length args > 0
          then getNumber symRef (head args)
          else return 1
  exitWith (ExitFailure (round code))
