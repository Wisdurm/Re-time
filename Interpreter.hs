module Interpreter where
import Parser
import Tokenizer
import qualified Data.Map as Map

data Symbol = SValue Integer | SObject Object

data Symtab = Symtab {
  members :: Map.Map String Symbol,
  parent :: Maybe Symtab }

data Object = ObjectE [Symbol] | ObjectI Ast | BuiltIn ([Symbol] -> Symtab -> Symbol)

-- Symtabs and objects

emptyObject :: Object
emptyObject = ObjectE [(SValue 0)]

createSymtab :: Symtab
createSymtab = Symtab (Map.fromList [("Add", (SObject (BuiltIn addition))),
                                     ("Mul", (SObject (BuiltIn multiply))),
                                     ("Fun", (SObject (ObjectE [SValue 2]))),
                                      ("Array", (SObject (ObjectE [SValue 1, SValue 2, SValue 3]))),
                                      ("Double", (SObject (ObjectI
                                                           (Call {aobj = Identifier {aname = "Mul"}, aargs = [Literal {avalue = Right 2.0}, Identifier {aname = "arg"}]})
                                                          ))),
                                      ("at", (SObject (ObjectI
                                                           (Call {aobj = Identifier {aname = "Add"}, aargs = [Identifier {aname = "arg1"}, Identifier {aname = "arg2"}]})
                                                          )))]) Nothing

updateSymtab :: Symtab -> String -> Symbol -> Symtab
updateSymtab (Symtab members parent) k v = Symtab (Map.insert k v members) parent

-- Interpretation

interpret :: Ast -> Symtab -> [Symbol] -> Symbol
interpret (Literal (Left str)) _ _ = SValue 1234 -- TODO: string to char (uint8) array
interpret (Literal (Right num)) _ _ = SValue (floor num) -- TODO: change SValue type
interpret (Identifier name) sym locals = let res = Map.lookup name (members sym)
                                             -- Since sym is not returned this is useless, somebody call a monad!
                                         in case res of Nothing -> if length locals > 0 then head locals
                                                                   else SValue 0
                                                        (Just sym) -> sym
interpret (Call fun args) sym locals = evaluate (interpret fun sym []) (go args locals) sym
  where go :: [Ast] -> [Symbol] -> [Symbol]
        go (x:[]) y = [interpret x sym y]
        go (x:xs) [] = (interpret x sym []) : go xs []
        go (x:xs) (y:ys) = let result = interpret x sym (y:ys)
                           in case result of (SValue int) -> result : go xs (y:ys)
                                             (SObject ojb) -> result : go xs ys

-- Evaluation

evaluate :: Symbol -> [Symbol] -> Symtab -> Symbol
evaluate (SValue val) _ _ = SValue val
evaluate (SObject (ObjectE members)) args sym = evaluate (last members) [] sym -- TODO: Side effects and args and literally everything
evaluate (SObject (ObjectI ast)) args sym = interpret ast sym args
evaluate (SObject (BuiltIn fun)) args sym = fun args sym

hardEvaluate :: Symbol -> [Symbol] -> Symtab -> Integer
hardEvaluate (SValue val) _ _ = val
hardEvaluate obj args sym = let res = evaluate obj args sym
                            in case res of (SValue val) -> val
                                           obj -> hardEvaluate obj [] sym

-- Builtins

addition :: [Symbol] -> Symtab -> Symbol
addition xs sym = SValue . sum . map (\x -> hardEvaluate x [] sym) $ xs

multiply :: [Symbol] -> Symtab -> Symbol
multiply xs sym = SValue (foldr (\x y -> x*y) 1 (map (\x -> hardEvaluate x [] sym) xs))

-- Interface
easyInterpret :: String -> Symbol
easyInterpret str = let ast = parse . tokenize $ str
                        in case ast of Nothing -> SValue 0
                                       (Just a) -> interpret a createSymtab []

-- Can't add Symbol to Show typeclass because function types
showSymbol :: Symbol -> String
showSymbol (SValue int) = "SValue " ++ show int
showSymbol (SObject (ObjectE members)) = "SObject (ObjectE [" ++ (foldr (\mem res -> (showSymbol mem) ++ " " ++ res) "" members)  ++ "])"
showSymbol (SObject (ObjectI ast)) = "SObject (ObjectI [" ++ (show ast) ++ "])"
showSymbol (SObject (BuiltIn _)) = "Built-in function"
