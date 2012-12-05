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
      it "" $ macroExpand mm "*" node `shouldBe` (Right (PrintNode (n 1)))
      where mm = fromList [ (("*", "print"), (["*"], ["a"], ((PrintNode (q "a")))))
                          , (("*", "puts") , (["*"], ["a"], (f (q "print") (q "a"))))
                          , (("*", "echo") , (["*"], ["a"], (f (q "puts") (q "a")))) ]
            node = (f (q "echo") (n 1))
      