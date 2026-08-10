module Interpreter where
import Parser
import Tokenizer
import qualified Data.Map as Map

data Symtab = Symtab {
  members :: Map.Map String (Either String Double),
  parent :: Maybe Symtab }
  deriving Show

createSymtab :: Symtab
createSymtab = Symtab (Map.empty) Nothing

updateSymtab :: Symtab -> String -> (Either String Double) -> Symtab
updateSymtab (Symtab members parent) k v = Symtab (Map.insert k v members) parent

interpret :: Ast -> Symtab -> Either String Double
interpret (Literal value) _ = value
interpret (Identifier name) sym = let res = Map.lookup name (members sym)
                                  in case res of Nothing -> interpret (Identifier name)
                                                            (updateSymtab sym name (Right 0))
                                                 (Just val) -> val
-- Hard coded for testing
interpret (Call "+" (x:y:_)) sym = case v1 of (Left str1) -> case v2 of (Left str2) -> Left (str1 ++ str2)
                                                                        (Right _) -> Right 0
                                              (Right int1) -> case v2 of (Right int2) -> Right (int1 + int2)
                                                                         (Left _) -> Right 0
  where v1 = interpret x sym
        v2 = interpret y sym
                
-- interpret (Call fun args) sym =

-- Interface
easyInterpret :: String -> Either String Double
easyInterpret str = let ast = parse . tokenize $ str
                        in case ast of Nothing -> (Right 0)
                                       (Just a) -> interpret a createSymtab
