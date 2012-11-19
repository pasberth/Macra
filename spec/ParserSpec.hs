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

cmpNode :: String -> Node -> Expectation
cmpNode program node =
  case parse runTimeExpr "(ParserSpec.hs)" program of
    Left x -> fail $ show x
    Right x -> x `shouldBe` node

spec :: Spec
spec = do

  describe "Macra.Parser" $ do


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