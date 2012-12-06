module Main (main, spec) where

import Control.Monad
import Control.Monad.Trans
import Test.Hspec
import Macra.Parser
import Text.ParserCombinators.Parsec

main :: IO ()
main = hspec spec

q = SymNode
c = CharNode
n = NumNode
nil = NilNode
cons = ConsNode
f = FuncallNode

cmpNode :: String -> Node -> Expectation
cmpNode program node =
  case parse runTimeExpr "(ParserSpec.hs)" program of
    Left x -> fail $ show x
    Right x -> x `shouldBe` node

cmpCNode :: String -> [CNode] -> Expectation
cmpCNode program cnodes =
  case parse compileTimeExpr "(ParserSpec.hs)" program of
    Left x -> fail $ show x
    Right x -> x `shouldBe` cnodes

spec :: Spec
spec = do

  describe "Macra.Parser" $ do

    describe "Coloninfix" $ do

      it "左再帰" $ do
        cmpNode "list :inject 0 :into fn"
          (f (f (q ":into")
                (f (f (q ":inject")
                      (q "list"))
                   (n 0)))
             (q "fn"))

      it "左右の引数は共に funcall expression" $ do
        cmpNode "f x :and g y" $ do
          (f (f (q ":and")
                (f (q "f") (q "x")))
             (f (q "g") (q "y")))


      it "左再帰で、左右の引数は共に funcall expression" $ do
        cmpNode "f x :or g y :and h z" $ do
          (f (f (q ":and")
                (f (f (q ":or")
                      (f (q "f") (q "x")))
                   (f (q "g") (q "y"))))
             (f (q "h") (q "z")))

    describe "Equal Arrow" $ do

      it "x と a を引数とする関数呼び出し" $ do
        cmpNode "x => a" (f (f (q "=>") (q "x")) (q "a"))

      it "右辺には関数呼び出しの構文が許される" $ do
        cmpNode "x => f a" (f (f (q "=>") (q "x")) (f (q "f") (q "a")))

      it "関数呼び出しより優先順位が高い" $ do
        cmpNode "f x => a" (f (q "f") (f (f (q "=>") (q "x")) (q "a")))

      it "関数呼び出しより優先順位が高く、右辺には関数呼び出しの構文が許される" $ do
        cmpNode "f x => g a" (f (q "f") (f (f (q "=>") (q "x")) (f (q "g") (q "a"))))

    describe "Hyphen Arrow" $ do

      it "x と a を引数とする関数呼び出し" $ do
        cmpNode "x -> a" (f (f (q "->") (q "x")) (q "a"))

      it "関数呼び出しより優先順位が高い" $ do
        cmpNode "f x -> a" (f (q "f") (f (f (q "->") (q "x")) (q "a")))

    describe "Comma" $ do

      it "x と a を引数とする関数呼び出し" $ do
        cmpNode "x , a" (f (f (q ",") (q "x")) (q "a"))

      it "関数呼び出しより優先順位が高い" $ do
        cmpNode "f x, a" (f (q "f") (f (f (q ",") (q "x")) (q "a")))

    describe "Brackets" $ do

      it "should contain at least one expresssion" $
        pending "hspec で ParseError を投げたとき pass するようなテストの書き方がわからない"

      it "[ 関数の呼び出し" $
        cmpNode "[a]" (f (q "[") (q "a"))

    describe "Brace" $ do

      it "{ 関数の呼び出し" $
        cmpNode "{a}" (f (q "{") (q "a"))

    describe "Parenthesis" $ do

      it "( 関数の呼び出し" $
        cmpNode "(a)" (f (q "(") (q "a"))


    describe "Number" $ do
      it "should parse 0" $
        cmpNode "0" (n 0.0)

      it "should parse 0.0" $
        cmpNode "0.0" (n 0.0)

      it "should parse 0.00" $
        cmpNode "0.00" (n 0.0)

      it "should parse 1" $
        cmpNode "1" (n 1)

      it "should parse 1.23" $
        cmpNode "1.23" (n 1.23)

      it "should parse 12.3" $
        cmpNode "12.3" (n 12.3)

      it "should raise ParseError when parsing 00" $
        -- 00 とかをエラーにする
        pending "hspec で ParseError を投げたとき pass するようなテストの書き方がわからない"

      it "should raise ParseError when parsing 00.0" $
        pending "hspec で ParseError を投げたとき pass するようなテストの書き方がわからない"

      it "should raise ParseError when parsing 0..0" $
        pending "hspec で ParseError を投げたとき pass するようなテストの書き方がわからない"

      it "should raise ParseError when parsing 0." $
        pending "hspec で ParseError を投げたとき pass するようなテストの書き方がわからない"

      it "should raise ParseError when parsing .0" $
        pending "hspec で ParseError を投げたとき pass するようなテストの書き方がわからない"


    describe "Char" $ do

      it "prefix is $'" $ do
        cmpNode "$'c" (c 'c')

      it "can contain Japanese" $ do
        cmpNode "$'あ" (c 'あ')

      it "should include spaces in between character literal and next token." $ do
        -- $'ab とかをエラーにする
        pending "hspec で ParseError を投げたとき pass するようなテストの書き方がわからない"

    describe "String" $ do

      it "文字列は文字を cons したものを返す" $ do
        cmpNode "\"hoge\"" (cons (c 'h') (cons (c 'o') (cons (c 'g') (cons (c 'e') nil))))

      it "日本語は1文字として扱う" $ do
        cmpNode "\"日本語\"" (cons (c '日') (cons (c '本') (cons (c '語') nil)))

    describe "Identifier" $ do

      it "can contain hyphen" $
        cmpNode "hoge-fuga" (q "hoge-fuga")

      it "ハイフンから始める事はできない" $
        pending "hspec で ParseError を投げたとき pass するようなテストの書き方がわからない"

      it "ハイフンで終わる事はできない" $
        pending "hspec で ParseError を投げたとき pass するようなテストの書き方がわからない"

      it "can contain underscore" $
        cmpNode "hoge_fuga" (q "hoge_fuga")

      it "アンダースコアで始める事ができる" $
        cmpNode "_hoge" (q "_hoge")

      it "アンダースコアで終わる事ができる" $
        cmpNode "hoge_" (q "hoge_")

    describe "#ifopt" $ do
      it "ifopt は必ず then と else のリストを持つ" $
        cmpCNode (concat [ "#ifopt unittest\n"
                         , "#[ test :about expr : * -> * -> * = !do expr test ]\n"
                         , "#else\n"
                         , "#[ test :about expr : * -> * -> * = expr ]\n"
                         , "#end\n" ])
                 [IfoptCNode "unittest" [MacDefCNode ":about"
                                                    ["*","*","*"]
                                                    ["test","expr"]
                                                    (DoNode (q "expr") (q "test"))]
                                       [MacDefCNode ":about"
                                                    ["*","*","*"]
                                                    ["test","expr"]
                                                    (q "expr")]]