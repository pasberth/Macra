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

q = SymNode
c = CharNode
n = NumNode
nil = NilNode
cons = ConsNode
f = FuncallNode

spec :: Spec
spec = do

  describe "Macra.Compiler" $ do

    describe "recursive expansion" $ do
      -- #[ print a : * -> * = !print a ]
      -- #[ puts a  : * -> * = print a ]
      -- #[ echo a  : * -> * = puts a ]
      -- echo 1
      it "" $ do
        mm <- liftIO $ mkMacroMap [ MacDefCNode "print" ["*"] ["a"] (PrintNode (q "a"))
                                  , MacDefCNode "puts" ["*"] ["a"] (f (q "print") (q "a"))
                                  , MacDefCNode "echo" ["*"] ["a"] (f (q "puts") (q "a"))]
        mm `shouldBe` (Right $ fromList [ (("*", "print"), (["*"], ["a"], (PrintNode (q "a"))))
                                        , (("*", "puts") , (["*"], ["a"], (PrintNode (q "a"))))
                                        , (("*", "echo") , (["*"], ["a"], (PrintNode (q "a")))) ])