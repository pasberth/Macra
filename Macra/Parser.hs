
module Macra.Parser where

data Identifier = SymId String | NilId

data Node = SymNode Identifier
          | CharNode Char
          | NumNode  Double
          | ListNode [Node]
          | IfNode Node Node
          | LambdaNode Node Node
          | FuncallNode Node Node
          | MaccallNode Node Node