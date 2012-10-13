
module Macra.Parser (Identifier(..),
                     ToplevelNode(..),
                     MacCxtNode(..),
                     CxtDefMNode(..),
                     MacParams(..),
                     Node(..),
                     SigList,
                     CxtId,
                     parse) where

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding (parse)

data Identifier = SymId String | NilId deriving (Show, Eq, Ord)

type ToplevelNodes = [ToplevelNode]
data ToplevelNode = MacCxtTLNode MacCxtNode
                  | EvalCxtTLNode Node
                  deriving (Show, Eq)

data MacCxtNode = CxtDefMNode CxtId CxtDefMNode
                | SigDefMNode Identifier SigList
                deriving (Show, Eq)

data CxtDefMNode = MacDefMCNode Identifier MacParams Node
                 deriving (Show, Eq)

type SigList = [CxtId]
type CxtId = String

data MacParams = MacParams MacParams MacParams
               | MacParam Identifier
               deriving (Show, Eq)

data Node = SymNode Identifier
          | CharNode Char
          | NumNode  Double
          | ListNode [Node]
          | IfNode Node Node
          | LambdaNode Identifier Node
          | DefineNode Identifier Node
          | ReturnNode Node
          | FuncallNode Node Node
          | MaccallNode Node Node
          | KwargNode Identifier Node
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
  show (KwargNode kw arg) = concat ["Kwarg ", show kw, " = ", (indent2 $ show arg)]

indent :: String -> String -> String
indent idt node = foldl (\str x -> concat [str, "\n", idt,  x]) "" (lines node)
indent2 node = indent "  " node

parse :: FilePath -> String -> Either ParseError ToplevelNodes
parse fname program =
      case P.parse parseProgram fname program of
            Left x -> Left x
            Right node -> Right node

parseMarkAsIdentifer :: Parser Identifier
parseMarkAsIdentifer = parseMarkAsIdentifer' <|> parseIdAsIdentifier
          where parseMarkAsIdentifer' = try $ do
                a <- beginLetter
                b <- many containLetter
                return $ SymId (a:b)
                -- ruby -e 'puts [*33..47, *58..64, *91..96, *123..126].map(&:chr).join'
                where beginLetter = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
                      containLetter = beginLetter


parseIdAsIdentifier :: Parser Identifier
parseIdAsIdentifier = try parseNilIdAsIdentifier <|> try parseSymIdAsIdentifier
                    where parseNilIdAsIdentifier = do { string "nil"; return NilId }
                          parseSymIdAsIdentifier = do
                                                 a <- beginLetter
                                                 b <- many containLetter
                                                 return $ SymId (a:b)
                                                 where beginLetter = letter
                                                       containLetter = letter <|> oneOf "0123456789" <|> oneOf "-"

parseMark :: Parser Node
parseMark = try $ do
          mark <- parseMarkAsIdentifer
          return $ SymNode mark

parseId :: Parser Node
parseId = try $ do
        id <- parseIdAsIdentifier
        return $ SymNode id

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

parseProgram :: Parser ToplevelNodes
parseProgram = do
             stats <- many $ do { expr <- parseMaccall
                                ; skipSpaces
                                ; do { string ";"; return () } <|> do { eof; return () }
                                ; skipSpaces
                                ; return $ EvalCxtTLNode expr }
             eof
             return stats

parseExpr :: Parser Node
parseExpr = parseKeywordArgument <?> "a expression"

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
                            return $ MaccallNode id
                   parseMaccall' = try $ do
                                 expr1 <- parseLambdaSyntax
                                 sfxes <- many ((try $ do {
                                       op <- maccall
                                       ; expr2 <- parseKeywordArgument
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
                                        ; arg1 <- try $ do { skipSpaces; parseMaccall >>= return }
                                        ; args <- (many $ do {
                                                        skipSpaces
                                                        ; string ";"
                                                        ; skipSpaces
                                                        ;  parseMaccall >>= return })
                                        ; skipSpaces
                                        ; string end
                                        ; return $ foldl (\a b -> MaccallNode a b) (SymNode $ SymId beg) (arg1:args)
                                    }
                          parseBracket = bracket "[" "]" <|>
                                       bracket "(" ")"

parseKeywordArgument = parseKeywordArgument' <|> parseLambdaSyntax <?> "keyword argument"
                     where parseKeywordArgument' = try $ do
                                                 kw <- parseIdAsIdentifier
                                                 string ":"
                                                 requireSpaces
                                                 arg <- parseExpr
                                                 return $ KwargNode kw arg

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
              id <- parseIdAsIdentifier
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
              id <- parseIdAsIdentifier
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

skipSpaces = skipMany (oneOf " \t\n") <?> "skipped spaces"
requireSpaces = skipMany1 (oneOf " \t\n") <?> "spaces"