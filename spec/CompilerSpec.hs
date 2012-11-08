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
                          [ (CxtDefMNode
                              "function"
                              [ (MacDefMCNode
                                  (SymId "m")
                                  [(SymId "x")]
                                  (SymNode (SymId "x")))
                              ])
                          ])
                      ]) `shouldBe`
                          fromList [
                            (("function", (SymId "m")), ([(SymId "x")],
                                                 (SymNode (SymId "x"))))
                          ]

    describe "macro expansion " $ do

      it "a" $ do
        (macroExpand (fromList
                       [ ( (toplevelContext, (SymId "m")),
                           ([(SymId "x")], (SymNode (SymId "x")))
                         )
                       ])
                     (fromList [])
                     (MaccallNode
                       (SymNode (SymId "m"))
                       (SymNode (SymId "a"))) `shouldBe`
                         (SymNode (SymId "a")))
      it "b" $ do
        (macroExpand (fromList
                       [ ( (toplevelContext, (SymId "m")),
                           ( [(SymId "x"), (SymId "y")],
                             (MaccallNode
                               (SymNode (SymId "x"))
                               (SymNode (SymId "y"))))
                         )
                       ])
                     (fromList [])
                     (MaccallNode
                       (MaccallNode
                         (SymNode (SymId "m"))
                         (SymNode (SymId "a")))
                       (SymNode (SymId "b"))) `shouldBe`
                         (MaccallNode
                           (SymNode (SymId "a"))
                           (SymNode (SymId "b"))))

      it "b" $ do
        (macroExpand (fromList
                       [ ( (toplevelContext, (SymId "=>"))
                         , ( [(SymId "a"), (SymId "b")]
                           , (SymNode (SymId "xxx"))))
                       , ( ("function", (SymId "=>"))
                         , ( [(SymId "a"), (SymId "b")]
                           , (SymNode (SymId "yyy"))))
                       ])
                     -- map :: function -> expression
                     (fromList
                       [ ( (SymId "map")
                         , ["function", "expression"])
                       ])

                     -- map x => x
                     (MaccallNode
                       (SymNode (SymId "map"))
                       (MaccallNode
                         (MaccallNode
                           (SymNode (SymId "=>"))
                           (SymNode (SymId "x")))
                         (SymNode (SymId "x")))) `shouldBe`
                         (FuncallNode
                           (SymNode (SymId "map"))
                           (SymNode (SymId "yyy"))))
