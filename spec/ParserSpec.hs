module Main (main, spec) where

import Control.Monad
import Control.Monad.Trans
import Test.Hspec
import Macra.Parser

main :: IO ()
main = hspec spec

cmpTLNode :: String -> ToplevelNode -> Expectation
cmpTLNode program node = do
  case parse "(ParserSpec.hs)" program of
    Left x -> fail (show x)
    Right x -> x `shouldBe` node

cmpNode :: String -> Node -> Expectation
cmpNode program node = do
  case parse "(ParserSpec.hs)" program of
    Left x -> fail (show x)
    Right (EvalCxtTLNode x) -> x `shouldBe` node

spec :: Spec
spec = do
  describe "Macra.Parser" $ do

    describe "#macro context" $ do
      it "" $ do
        cmpTLNode (concat [ "#macro\n",
                            "#context function\n",
                            "m x = x\n",
                            "#end\n",
                            "#end"]) (MacCxtTLNode
                                       (CxtDefMNode
                                         "function"
                                         (MacDefMCNode
                                           (SymId "m")
                                           (MacParam (SymId "x"))
                                           (SymNode (SymId "x")))))

    describe "Arrow Syntax and Comma Syntax" $ do

      it "優先順位は同じ case 1" $ do
        cmpNode "x,y,z => a" (MaccallNode
                               (MaccallNode
                                 (SymNode (SymId ","))
                                 (SymNode (SymId "x")))
                               (MaccallNode
                                 (MaccallNode
                                   (SymNode (SymId ","))
                                   (SymNode (SymId "y")))
                                 (MaccallNode
                                   (MaccallNode
                                     (SymNode (SymId "=>"))
                                     (SymNode (SymId "z")))
                                   (SymNode (SymId "a")))))

      it "優先順位は同じ case 2" $ do
        cmpNode "x => y, z => a" (MaccallNode
                               (MaccallNode
                                 (SymNode (SymId "=>"))
                                 (SymNode (SymId "x")))
                               (MaccallNode
                                 (MaccallNode
                                   (SymNode (SymId ","))
                                   (SymNode (SymId "y")))
                                 (MaccallNode
                                   (MaccallNode
                                     (SymNode (SymId "=>"))
                                     (SymNode (SymId "z")))
                                   (SymNode (SymId "a")))))

    describe "Bracket Syntax" $ do

      it "be separated by ';'" $ do
        cmpNode "(a; b; c)" (MaccallNode
                              (MaccallNode
                                (MaccallNode
                                  (SymNode (SymId "("))
                                  (SymNode (SymId "a")))
                                (SymNode (SymId "b")))
                              (SymNode (SymId "c")))

    describe "Define Syntax" $ do

      it "keyword is '!define'" $ do
        cmpNode "!define x y" (DefineNode
                                (SymId "x")
                                (SymNode (SymId "y")))

    describe "Lambda Syntax" $ do

      it "keyword is '!lambda'" $ do
         cmpNode "!lambda x y" (LambdaNode
                                 (SymId "x")
                                 (SymNode (SymId "y")))

    describe "If Syntax" $ do

      it "keyword is '!if'" $ do
         cmpNode "!if cond then" (IfNode
                                   (SymNode (SymId "cond"))
                                   (SymNode (SymId "then")))

    describe "Funcall Syntax" $ do

      it "keyword is '!if'" $ do
        cmpNode "!funcall f x" (FuncallNode
                                 (SymNode (SymId "f"))
                                 (SymNode (SymId "x")))

      -- うまい英語に書き換えてくれ
      it "カリー化のため!funcall f x の f として !funcall g y も許される" $ do
        cmpNode "!funcall !funcall f x y" (FuncallNode
                                            (FuncallNode
                                              (SymNode (SymId "f"))
                                              (SymNode (SymId "x")))
                                            (SymNode (SymId "y")))

      it "構文は2引数固定なので、!funcall f xのxとして!funcall g yも許される" $ do
        cmpNode "!funcall f !funcall g x" (FuncallNode
                                            (SymNode (SymId "f"))
                                            (FuncallNode
                                              (SymNode (SymId "g"))
                                              (SymNode (SymId "x"))))

    describe "Infix Maccall Syntax" $ do

      it "" $ do
        cmpNode "then :if cond" (MaccallNode
                                  (MaccallNode
                                    (SymNode (SymId "if"))
                                    (SymNode (SymId "then")))
                                  (SymNode (SymId "cond")))

      it "中値演算子として:+のような記号も許される" $ do

        cmpNode "x :+ y" (MaccallNode
                           (MaccallNode
                             (SymNode (SymId "+"))
                             (SymNode (SymId "x")))
                           (SymNode (SymId "y")))

      it "[a-zA-Z]もidとして許されるけど、記号で始まる場合は含めない方がよくない？" $ do

        cmpNode "x :<+>y z" (MaccallNode
                              (MaccallNode
                                (MaccallNode
                                  (SymNode (SymId "<+>"))
                                  (SymNode (SymId "x")))
                                 (SymNode (SymId "y")))
                               (SymNode (SymId "z")))

      it "" $ do
        cmpNode "map x => y :* z" (MaccallNode
                                    (SymNode (SymId "map"))
                                    (MaccallNode
                                      (MaccallNode
                                        (SymNode (SymId "=>"))
                                        (SymNode (SymId "x")))
                                      (MaccallNode
                                        (MaccallNode
                                          (SymNode (SymId "*"))
                                          (SymNode (SymId "y")))
                                        (SymNode (SymId "z")))))

      it "" $ do
        cmpNode "a b @f" (MaccallNode
                           (SymNode (SymId "f"))
                           (MaccallNode
                             (SymNode (SymId "a"))
                             (SymNode (SymId "b"))))