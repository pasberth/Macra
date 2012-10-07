
module Macra.Parser where

data Node = Sym String
          | Char String
          | Num  String
          | List [Node]
          | If Node Node
          | Lambda Node Node
          | Funcall Node Node
          | Maccall Node Node