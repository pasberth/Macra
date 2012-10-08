
module Macra.Parser (Identifier(..), Node(..), parse) where

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding (parse)

data Identifier = SymId String | NilId deriving (Show, Eq)

data Node = SymNode Identifier
          | CharNode Char
          | NumNode  Double
          | ListNode [Node]
          | IfNode Node Node
          | LambdaNode Node Node
          | DefineNode Node Node
          | ReturnNode Node
          | FuncallNode Node Node
          | MaccallNode Node Node
          deriving (Eq)

instance Show Node where
  show (SymNode NilId) = "#<nil>"
  show (SymNode (SymId sym)) = concat ["'", sym]
  show (CharNode c) = show c
  show (NumNode n) = show n
  show (ListNode l) = show l
  show (IfNode a b) = concat ["!if", (indent2 $ show a), (indent2 $ show b)]
  show (LambdaNode a b) = concat ["!lambda", (indent2 $ show a), (indent2 $ show b)]
  show (DefineNode a b) = concat ["!define", (indent2 $ show a), (indent2 $ show b)]
  show (ReturnNode a) = concat ["!return", (indent2 $ show a)]
  show (FuncallNode a b) = concat ["!funcall", (indent2 $ show a), (indent2 $ show b)]
  show (MaccallNode a b) = concat ["#maccall", (indent2 $ show a), (indent2 $ show b)]

indent :: String -> String -> String
indent idt node = foldl (\str x -> concat [str, "\n", idt,  x]) "" (lines node)
indent2 node = indent "  " node

parse :: FilePath -> String -> Either ParseError Node
parse fname program =
      case P.parse parseProgram fname program of
            Left x -> Left x
            Right node -> Right node

parseMark :: Parser Node
parseMark = try $ do
          id <- symbol
          return $ SymNode (SymId id)
          where symbol = many1 (noneOf " \t\n")
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
parseExpr = parseLambdaSyntax <?> "a expression"

a :: (Node -> Node) -> Node -> Node -> Node
a f n m = MaccallNode (f n) m

parseMaccall :: Parser Node
parseMaccall = parseMaccall' <?> "one of prefix/infix/suffix"
             where maccall = infixOp <|> prefixOp
                   prefixOp = try $ do
                            requireSpaces
                            return MaccallNode
                   infixOp = try $ do
                           skipSpaces
                           string ":"
                           id <- parseMark
                           skipSpaces
                           return $ a (MaccallNode id)
                   suffixOp = try $ do
                            skipSpaces
                            string "@"
                            id <- parseMark
                            skipSpaces
                            return $ MaccallNode id
                   parseMaccall' = try $ do
                                 expr1 <- parseLambdaSyntax
                                 sfxes <- many ((try $ do {
                                       op <- maccall
                                       ; expr2 <- parseLambdaSyntax
                                       ; return $ (\node -> op node expr2)
                                       }) <|> (try $ do {
                                         op <- suffixOp
                                         ; return op
                                       }))
                                 return $ foldl (\expr sfx -> sfx expr) expr1 sfxes

parseBracketMaccall :: Parser Node
parseBracketMaccall = parseBracket <|> parseVMInst <|> parseId <|> parseNumber
                    where bracket beg end = try $ do {
                                        string beg
                                        ; args <- (many $ do { skipSpaces; parseExpr >>= return })
                                        ; skipSpaces
                                        ; string end
                                        ; return $ MaccallNode (SymNode $ SymId beg) (ListNode args)
                                    }
                          parseBracket = bracket "[" "]" <|>
                                       bracket "(" ")"

parseLambdaSyntax :: Parser Node
parseLambdaSyntax = parseEqualArrow <|> parseComma <|> parseBracketMaccall

parseEqualArrow :: Parser Node
parseEqualArrow = try (do
                expr1 <- parseBracketMaccall
                skipSpaces
                string "=>"
                skipSpaces
                expr2 <- parseMaccall
                return $ MaccallNode (MaccallNode (SymNode $ SymId "=>") expr1) expr2
                ) <?> "`=>'"

-- case 1
-- x,y,z => a
-- (x , (y , (z => a))
-- x,y => (z => a)
-- x => (y => (z => a))

-- case 2
-- x => y, z => a
-- x => (y , (z => a))
-- x => (y => (z => a))
parseComma :: Parser Node
parseComma = try (do
           expr1 <- parseBracketMaccall
           skipSpaces
           string ","
           skipSpaces
           expr2 <- parseMaccall
           return $ MaccallNode (MaccallNode (SymNode $ SymId ",") expr1) expr2
           ) <?> "`,'"

parseVMInst :: Parser Node
parseVMInst = parseVMIf <|> parseVMLambda <|> parseVMReturn <|> parseVMDefine <|> parseVMFuncall

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
              requireSpaces
              expr <- parseExpr
              return $ ReturnNode expr

parseVMDefine :: Parser Node
parseVMDefine = try $ do
              string "!define"
              requireSpaces
              id <- parseId
              skipSpaces
              expr <- parseExpr
              return $ DefineNode id expr

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