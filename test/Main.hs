{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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

    it "parses call calls" $ do
      Ast.parse (Token.tokenize "Add(1 2)()") `shouldBe` Just (
        Ast.Call (Ast.Call (Ast.Identifier "Add")
                  [Ast.Literal (Right 1), Ast.Literal (Right 2)]) []
        )

      Ast.parse (Token.tokenize "Test(1 2)(3)()") `shouldBe` Just (
        Ast.Call (Ast.Call (Ast.Call (Ast.Identifier "Test")
                            [Ast.Literal (Right 1), Ast.Literal (Right 2)])
                  [Ast.Literal (Right 3)]) []
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

    it "handles call calls" $ do
      str <- interpretText "Series( \
                           \  Set(x 2) \
                           \  Set(i 5) \
                           \  While(<(0 i) Object(Set(x *(x 2)) \
                           \                      Set(i -(i 1)))() ) \
                           \  x)"
      str `shouldBe` "64.0"

  describe "Retime.Interpreter.Libraries.Standard" $ do
    it "implements Series" $ do
      str <- interpretText "Series(1 2 3)"
      str `shouldBe` "3.0"

      str <- interpretText "Series(Set(x 3) 2 1 x)"
      str `shouldBe` "3.0"

      str <- interpretText "Series()"
      str `shouldBe` "[]"

    it "implements Copy" $ do
      str <- interpretText "Copy(x 14)"
      str `shouldBe` "14.0"

      str <- interpretText "Series( \
                           \ Copy(x 15) \
                           \ Copy(y x) \
                           \ y)"
      str `shouldBe` "15.0"

      str <- interpretText "Series(Set(x 0) Copy(x))"
      str `shouldBe` "0.0"

      str <- interpretText "Copy()"
      str `shouldBe` "[]"

    it "implements Convert" $ do
      str <- interpretText "Convert(x 1 2 3)"
      str `shouldBe` "[1.0 2.0 3.0]"

      str <- interpretText "Convert(x)"
      str `shouldBe` "[]"

    it "implements Set" $ do
      str <- interpretText "Set(x 14)"
      str `shouldBe` "14.0"

      str <- interpretText "Series( \
                           \ Set(x 15) \
                           \ Set(y x) \
                           \ y)"
      str `shouldBe` "15.0"

      str <- interpretText "Set(x Object(1 2 3))"
      str `shouldBe` "[1.0 2.0 3.0]"

      str <- interpretText "Series(Set(x 0) Set(x))"
      str `shouldBe` "0.0"

      str <- interpretText "Set()"
      str `shouldBe` "[]"

    it "implements Object" $ do
      str <- interpretText "Object(1 2 3)"
      str `shouldBe` "[1.0 2.0 3.0]"

      str <- interpretText "Object()"
      str `shouldBe` "[]"

    it "implements If" $ do
      str <- interpretText "If(Nil 1 2)"
      str `shouldBe` "2.0"

      str <- interpretText "If(3 1 2)"
      str `shouldBe` "1.0"

      str <- interpretText "If(3 1)"
      str `shouldBe` "1.0"

      str <- interpretText "If(Nil 1)"
      str `shouldBe` "[]"

      str <- interpretText "If(Nil)"
      str `shouldBe` "[]"
      str <- interpretText "If()"
      str `shouldBe` "[]"

    it "implements Not" $ do
      str <- interpretText "Not(1)"
      str `shouldBe` "[]"

      str <- interpretText "Not(Nil)"
      str `shouldBe` "1.0"

      str <- interpretText "Not()" -- I am proud of myself
      str `shouldBe` "1.0"

    it "implements Head" $ do
      str <- interpretText "Head(Object(1 2 3))"
      str `shouldBe` "1.0"

      str <- interpretText "Head()"
      str `shouldBe` "[]"

    it "implements Tail" $ do
      str <- interpretText "Tail(Object(1 2 3))"
      str `shouldBe` "[2.0 3.0]"

      str <- interpretText "Tail()"
      str `shouldBe` "[]"

    it "implements List" $ do
      str <- interpretText "Series(Set(xs Object(2 3)) List(1 xs)"
      str `shouldBe` "[1.0 2.0 3.0]"

      str <- interpretText "List(1)"
      str `shouldBe` "1.0"

      str <- interpretText "List()"
      str `shouldBe` "[]"

    it "implements While" $ do
      -- Call-calls test already tests functionality so
      -- here we just test edge cases.
      -- Dont test While(1) since that should freeze indefinitely,
      -- it only makes sense.

      str <- interpretText "While()"
      str `shouldBe` "[]"

      str <- interpretText "While(Nil)"
      str `shouldBe` "[]"

    it "implements Read" $ do
      str <- interpretText "Read(\"2\")"
      str `shouldBe` "2.0"

      str <- interpretText "Read(\"2,5\")"
      str `shouldBe` "2.5"

      str <- interpretText "Read(\"moi\")"
      str `shouldBe` "[]"

  describe "Retime.Interpreter.Libraries.Math" $ do
    it "implements Add" $ do
      str <- interpretText "+(1 2)"
      str `shouldBe` "3.0"

      str <- interpretText "+(1 2 3)"
      str `shouldBe` "6.0"

      str <- interpretText "+(1)"
      str `shouldBe` "1.0"

      str <- interpretText "+()"
      str `shouldBe` "0.0"

    it "implements Negate" $ do
      str <- interpretText "-(2 1)"
      str `shouldBe` "1.0"

      str <- interpretText "-(3 2 1)"
      str `shouldBe` "0.0"

      str <- interpretText "-(1)"
      str `shouldBe` "-1.0"

      str <- interpretText "-()"
      str `shouldBe` "0.0"

    it "implements Multiply" $ do
      str <- interpretText "*(2 3)"
      str `shouldBe` "6.0"

      str <- interpretText "*(3 2 4)"
      str `shouldBe` "24.0"

      str <- interpretText "*(2)"
      str `shouldBe` "2.0"

      -- Google identity element
      str <- interpretText "*()"
      str `shouldBe` "1.0"

    it "implements Compare" $ do
      str <- interpretText "=(2 3)"
      str `shouldBe` "[]"

      str <- interpretText "=(2 2)"
      str `shouldBe` "2.0"

      str <- interpretText "=(3 3 3)"
      str `shouldBe` "3.0"

      str <- interpretText "=(3 3 4 3)"
      str `shouldBe` "[]"

      str <- interpretText "=(2)"
      str `shouldBe` "2.0"

      str <- interpretText "=()"
      str `shouldBe` "[]"

    it "implements Smaller" $ do
      str <- interpretText "<(2 3)"
      str `shouldBe` "2.0"

      str <- interpretText "<(3 2)"
      str `shouldBe` "[]"

      str <- interpretText "<(1 2 3)"
      str `shouldBe` "1.0"

      str <- interpretText "<(1 3 2)"
      str `shouldBe` "[]"

      str <- interpretText "<(2)"
      str `shouldBe` "2.0"

      str <- interpretText "<()"
      str `shouldBe` "[]"
