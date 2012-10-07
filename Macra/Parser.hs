
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
          | ReturnNode
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
parseSymId = try $ do
           id <- symbol
           return $ SymNode (SymId id)
           where symbol = do
                        a <- beginLetter
                        b <- many containLetter
                        return (a:b)
                 beginLetter = letter
                 containLetter = letter <|> oneOf "0123456789" <|> oneOf "-"

parseNilId :: Parser Node
parseNilId = try $ do
           string "nil"
           return $ SymNode NilId

parseNumber :: Parser Node
parseNumber = parseFloatNum <|> parseIntNumAsFloat <?> "a number"

parseFloatNum = try $ do
            i <- parseIntNum
            char '.'
            ds <- many1 digit
            return $ NumNode (read $ concat [show i, ".", ds])
            where digit = oneOf "0123456789"

parseIntNumAsFloat = try $ do
                   i <- parseIntNum
                   return $ NumNode (read $ concat [show i, ".", "0"])

parseIntNum :: Parser Integer
parseIntNum = parseIntNumNonZero <|> parseIntNumZero <?> "a integer"
parseIntNumZero = try $ do { char '0'; return 0 }
parseIntNumNonZero = try $ do
            sign <- char '-' <|> do {return ' '}
            d <- beginDigit
            ds <- many digit
            return $ read $ concat [[sign], [d], ds]
            where digit = oneOf "0123456789"
                  beginDigit = oneOf "123456789"

parseProgram :: Parser Node
parseProgram = parseMaccall

parseExpr :: Parser Node
parseExpr = parseVMInst <|> parseId <|> parseNumber <?> "a expression"

a :: (Node -> Node) -> Node -> Node -> Node
a f n m = MaccallNode n (f m)

parseMaccall = parsePrefixMaccall

parsePrefixMaccall :: Parser Node
parsePrefixMaccall = try $ parseInfixMaccall `chainl1` prefix
                   where prefix = try $ do
                                requireSpaces
                                return MaccallNode

parseInfixMaccall :: Parser Node
parseInfixMaccall = try $ parseBracketMaccall `chainl1` infixOp
                  where infixOp = try $ do
                                string ":"
                                id <- parseSymId
                                skipSpaces
                                return $ a (MaccallNode id)

parseBracketMaccall :: Parser Node
parseBracketMaccall = parseExpr

parseVMInst :: Parser Node
parseVMInst = parseVMIf <|> parseVMLambda <|> parseVMReturn <|> parseVMAssign <|> parseVMFuncall

parseVMIf :: Parser Node
parseVMIf = try $ do
          string "!if"
          requireSpaces
          a <- parseExpr
          skipSpaces
          b <- parseExpr
          return $ IfNode a b

parseVMLambda :: Parser Node
parseVMLambda = try $ do
              string "!lambda"
              requireSpaces
              id <- parseId
              skipSpaces
              expr <- parseExpr
              return $ LambdaNode id expr

parseVMReturn :: Parser Node
parseVMReturn = try $ do
              string "!return"
              return ReturnNode

parseVMAssign :: Parser Node
parseVMAssign = try $ do
              string "!assign"
              requireSpaces
              id <- parseId
              skipSpaces
              expr <- parseExpr
              return $ AssignNode id expr

parseVMFuncall :: Parser Node
parseVMFuncall = try $ do
               string "!funcall"
               requireSpaces
               f <- parseExpr
               skipSpaces
               a <- parseExpr
               return $ FuncallNode f a

skipSpaces = skipMany (oneOf " \t\n")
requireSpaces = skipMany1 (oneOf " \t\n")