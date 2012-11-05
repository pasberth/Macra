module Main (main, spec) where

import Data.Map
import Control.Monad.State
import Control.Monad
import Control.Monad.Trans
import Test.Hspec
import Macra.Parser
import Macra.Compiler

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Macra.Compiler" $ do

    describe "macro defination" $ do

      it "a" $ do

         (macroDefine [ (MacCxtTLNode
                          (CxtDefMNode
                            "function"
                            (MacDefMCNode
                              (SymId "m")
                              (MacParam (SymId "x"))
                              (SymNode (SymId "x")))))
                      ]) `shouldBe`
                          fromList [
                            (("function", (SymId "m")), ((MacParam (SymId "x")),
                                                 (SymNode (SymId "x"))))
                          ]
    describe "macro expansion" $ do

      it "a" $ do
        (macroExpand emptyMacroMap
                     toplevelContext
                     (MaccallNode
                       (MaccallNode
                         (SymNode (SymId "a"))
                         (SymNode (SymId "b")))
                       (SymNode (SymId "c")))) `shouldBe`
                         (FuncallNode
                           (FuncallNode
                             (SymNode (SymId "a"))
                             (SymNode (SymId "b")))
                           (SymNode (SymId "c")))