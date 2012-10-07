
module Macra.Parser where

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding (parse)

data Identifier = SymId String | NilId deriving (Show, Eq)

data Node = SymNode Identifier
          | CharNode Char
          | NumNode  Double
          | ListNode [Node]
          | IfNode Node Node
          | LambdaNode Node Node
          | AssignNode Node Node
          | FuncallNode Node Node
          | MaccallNode Node Node
          deriving (Show, Eq)

parse :: FilePath -> String -> Either ParseError Node
parse fname program =
      case P.parse parseProgram fname program of
            Left x -> Left x
            Right node -> Right node

parseId :: Parser Node
parseId = parseNilId <|> parseSymId

parseSymId :: Parser Node
parseSymId = do
           id <- symbol
           return $ SymNode (SymId id)
           where symbol = do
                        a <- beginLetter
                        b <- many containLetter
                        return (a:b)
                 beginLetter = letter
                 containLetter = letter <|> oneOf "0123456789" <|> oneOf "-"

parseNilId :: Parser Node
parseNilId = do
           try $ string "nil"
           return $ SymNode NilId

parseProgram :: Parser Node
parseProgram = parseVMAssign

parseVMInst :: Parser Node
parseVMInst = parseVMAssign

parseVMAssign :: Parser Node
parseVMAssign = do
              string "!assign"
              skipSpaces
              id <- parseId
              skipSpaces
              expr <- parseExpr
              return $ AssignNode id expr

parseExpr = parseSymId
skipSpaces = skipMany (oneOf " \t\n")