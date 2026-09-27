{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Retime.Tokenizer as Token
import qualified Retime.Parser as Ast
-- import qualified Retime.Interpreter as Intp
import Retime
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Retime.Tokenizer.tokenize" $ do
    it "recognizes identifiers" $ do
      Token.tokenize "moi" `shouldBe` [Token.Identifier "moi"]

      Token.tokenize "+" `shouldBe` [Token.Identifier "+"]

    it "recognizes string literals" $ do
      Token.tokenize "Se \"on\" kissa" `shouldBe`
        [Token.Identifier "Se", Token.Literal "on", Token.Identifier "kissa"]

    it "recognizes number literals" $ do
      Token.tokenize "On 12 kissaa" `shouldBe`
        [Token.Identifier "On", Token.Number "12", Token.Identifier "kissaa"]

    it "recognizes punctuation" $ do
      Token.tokenize "Print()" `shouldBe`
        [Token.Identifier "Print", Token.Punctuation "(", Token.Punctuation ")"]

    it "ignores whitespace" $ do
      Token.tokenize "\t  \nPrint \t\n (  )  " `shouldBe`
        [Token.Identifier "Print", Token.Punctuation "(", Token.Punctuation ")"]

    it "ignores comments" $ do
      Token.tokenize "Se #on\n # Moikka kaikki!!\n kissa" `shouldBe`
        [Token.Identifier "Se", Token.Identifier "kissa"]

    it "can parse arguments" $ do
      Token.tokenize "Print(Object(1 x)s \"hi\" )" `shouldBe`
        [Token.Identifier "Print", Token.Punctuation "(", Token.Identifier "Object",
         Token.Punctuation "(", Token.Number "1", Token.Identifier "x",
         Token.Punctuation ")", Token.Identifier "s", Token.Literal "hi",
          Token.Punctuation ")"]

  describe "Retime.Parser.parse" $ do
    it "parses identifiers" $ do
      Ast.parse (Token.tokenize "moi") `shouldBe` Just (Ast.Identifier "moi")

    it "parses literals" $ do
      Ast.parse (Token.tokenize "12") `shouldBe` Just (Ast.Literal (Right 12))
      Ast.parse (Token.tokenize "\"moi\"") `shouldBe` Just (Ast.Literal (Left "moi"))

    it "parses simple calls" $ do
      Ast.parse (Token.tokenize "Add(1 2)") `shouldBe` Just (
        Ast.Call (Ast.Identifier "Add") [Ast.Literal (Right 1), Ast.Literal (Right 2)]
        )

    it "parses complex calls" $ do
      Ast.parse (Token.tokenize "Fst(Snd(1 Thr(2 3)) 4)") `shouldBe` Just (
        Ast.Call (Ast.Identifier "Fst")
          [Ast.Call (Ast.Identifier "Snd") [Ast.Literal (Right 1),
                                            Ast.Call (Ast.Identifier "Thr") [
                                               Ast.Literal (Right 2),
                                               Ast.Literal (Right 3)
                                                                            ]
                                           ]
          , Ast.Literal (Right 4)]
        )

  describe "Retime.Interpreter.interpret" $ do
    it "parses numbers" $ do
      str <- interpretText "2"
      str `shouldBe` "2.0"

    it "parses strings" $ do
      str <- interpretText "\"moi\""
      str `shouldBe` "[109.0 111.0 105.0]"

    it "handles empty objects" $ do
      str <- interpretText "obj"
      str `shouldBe` "[]"

    it "handles objects" $ do
      str <- interpretText "Object(1 2 3)"
      str `shouldBe` "[1.0 2.0 3.0]"

    it "handles Copy" $ do
      str <- interpretText "Copy(x 14)"
      str `shouldBe` "14.0"

      str <- interpretText "Series( \
                           \ Copy(x 15) \
                           \ Copy(y x) \
                           \ y)"
      str `shouldBe` "15.0"

    it "handles functions" $ do
      str <- interpretText "Series( \
                           \  Set(f Object(+(x y))) \
                           \  f(1 2) \
                           \ )"
      str `shouldBe` "3.0"

      str <- interpretText "Series( \
                           \  Copy(f +(x y)) \
                           \  f(2 3) \
                           \ )"
      str `shouldBe` "5.0"

    it "handles first class functions" $ do
      str <- interpretText "Series( \
                           \ Copy(CallFunc func(3)) \
	                   \ Set(Callback Object(Series(x) \
			   \                     Set(y *(x 2)))) \
                           \ CallFunc(Callback) \
                           \ )"
      str `shouldBe` "6.0"

    it "handles recursion" $ do
      str <- interpretText "Series( \
                           \  Set(Pow Object( \
		           \    Series(n k) \
			   \    If(=(k 1) \
			   \        n \
			   \      Series(Set(x -(k 1)) \
			   \             Set(r Pow(n x)) \
			   \             *(n r))))) \
                           \  Pow(2 4) \
                           \ )"
      str `shouldBe` "16.0"
