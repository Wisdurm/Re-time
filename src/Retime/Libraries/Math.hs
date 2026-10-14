module Retime.Libraries.Math where

import Retime.Interpreter.Types
import Retime.Interpreter.Convert
import Data.IORef

-- | Add all arguments
bAdd :: Object
bAdd = BuiltIn $ \args symRef -> do
  nums <- mapM (getNumber symRef) args
  newIORef (Right . sum $ nums)

-- | Negate all arguments
bNegate :: Object
bNegate = BuiltIn $ \args symRef -> do
  if length args == 1 then do
    num <- getNumber symRef (head args)
    newIORef (Right . negate $ num)
    else if length args > 0 then do
    nums <- mapM (getNumber symRef) args
    newIORef (Right (head nums - (sum . tail $ nums)))
    else valueZero

-- | Multiply all arguments
bMultiply :: Object
bMultiply = BuiltIn $ \args symRef -> do
  nums <- mapM (getNumber symRef) args
  -- Could do with monoids but dont want import for 1 line...
  newIORef (Right (foldr (\x y -> x * y) 1 nums))

-- | Compare arguments as numbers
bCompare :: Object
bCompare = BuiltIn $ \args symRef -> do
  nums <- mapM (getNumber symRef) args
  if allSame nums then
    case take 1 args of
      [] -> emptyObject
      [v] -> return v
    else emptyObject
    where allSame [] = True -- NOT SUPER EFFICIENT BUT GOOD FOR NOW
          allSame (_:[]) = True
          allSame (x:y:xs)
            | x == y = allSame (y:xs)
            | otherwise = False

-- | Checks if every argument is larger than their predecessor
bSmaller :: Object
bSmaller = BuiltIn $ \args symRef -> do
  nums <- mapM (getNumber symRef) args
  if comp nums then case take 1 args of
    [] -> emptyObject
    [v] -> return v
    else emptyObject
  where comp (x:y:ys)
          | x < y = comp (y:ys)
          | otherwise = False
        comp _ = True
