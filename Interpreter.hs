module Interpreter where
import Parser
import Tokenizer
import qualified Data.Map as Map

data Symbol = SValue Integer | SObject Object

data Symtab = Symtab {
  members :: Map.Map String Symbol,
  parent :: Maybe Symtab }

data Object = Object [Symbol] | BuiltIn ([Symbol] -> Symbol)

-- Symtabs and objects

emptyObject :: Object
emptyObject = Object [(SValue 0)]

createSymtab :: Symtab
createSymtab = Symtab (Map.fromList [("Add", (SObject (BuiltIn addition)))]) Nothing

updateSymtab :: Symtab -> String -> Symbol -> Symtab
updateSymtab (Symtab members parent) k v = Symtab (Map.insert k v members) parent

-- Interpretation

interpret :: Ast -> Symtab -> [Symbol] -> Symbol
interpret (Literal (Left str)) _ _ = SValue 1234 -- TODO: string to char (uint8) array
interpret (Literal (Right num)) _ _ = SValue (floor num) -- TODO: change SValue type
interpret (Identifier name) sym locals = let res = Map.lookup name (members sym)
                                         in case res of Nothing -> interpret (Identifier name) -- TODO: Use locals if can for new objects
                                                                   (updateSymtab sym name (SObject emptyObject))
                                                                   []
                                                        (Just sym) -> sym
interpret (Call fun args) sym _ = evaluate (interpret fun sym []) (map (\ast -> interpret ast sym []) args)

-- Evaluation

evaluate :: Symbol -> [Symbol] -> Symbol
evaluate (SValue val) _ = SValue val
evaluate (SObject (Object members)) args = SValue 1234 -- TODO: Functions
evaluate (SObject (BuiltIn fun)) args = fun args

hardEvaluate :: Symbol -> [Symbol] -> Integer
hardEvaluate (SValue val) _ = val
hardEvaluate obj args = let res = evaluate obj args
                        in case res of (SValue val) -> val
                                       obj -> hardEvaluate obj []

-- Builtins

addition :: [Symbol] -> Symbol
addition xs = SValue . sum . map (\x -> hardEvaluate x []) $ xs

-- Interface
easyInterpret :: String -> Symbol
easyInterpret str = let ast = parse . tokenize $ str
                        in case ast of Nothing -> SValue 0
                                       (Just a) -> interpret a createSymtab []

-- Can't add Symbol to Show typeclass because function types
showSymbol :: Symbol -> String
showSymbol (SValue int) = "SValue " ++ show int
showSymbol (SObject (Object members)) = "SObject (Object [" ++ (foldr (\mem res -> (showSymbol mem) ++ " " ++ res) "" members)  ++ "])"
showSymbol (SObject (BuiltIn _)) = "Builtin function"
