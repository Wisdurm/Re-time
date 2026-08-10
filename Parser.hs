module Parser where
import Tokenizer

data Ast = Call { aname :: String, aargs :: [Ast]}
         | Literal { avalue :: Either String Double }
         | Identifier { aname :: String }
         deriving Show

parse :: [Token] -> Maybe Ast
parse ((Token str TIdentifier):(Token "(" TPunctuation:xs)) = Just (Call { aname = str,
                                                                           aargs = go xs })
  where
    go :: [Token] -> [Ast]
    go (x:xs) = let res = parse (x:xs)
                in case res of Nothing -> []
                               (Just val) -> val : go xs
parse ((Token str TIdentifier):xs) = Just (Identifier str)
parse ((Token str TString):xs) = Just (Literal (Left str))
parse ((Token str TNumber):xs) = Just (Literal (Right (read str :: Double)))
parse ((Token ")" TPunctuation):_) = Nothing
parse _ = Nothing
