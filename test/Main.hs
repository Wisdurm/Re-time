{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Retime.Tokenizer as Token
import qualified Retime.Parser as Ast
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Retime.Tokenizer.tokenize" $ do
    it "recognizes identifiers" $ do
      Token.tokenize "moi" `shouldBe` [Token.Identifier "moi"]

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
