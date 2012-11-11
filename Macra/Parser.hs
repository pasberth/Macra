module Macra.Parser (Identifier(..),
                     ToplevelNode(..),
                     MacCxtNode(..),
                     Node(..),
                     CxtId,
                     MacSig,
                     MacParams,
                     parse) where

import Control.Monad
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding (parse)

type Identifier = String
data ToplevelNode = MacCxtTLNode MacCxtNode
                  | EvalCxtTLNode Node
                  deriving (Show, Eq)

data MacCxtNode = MacDef1MNode Identifier MacSig MacParams Node
                | MacDef2MNode Identifier MacSig MacParams
                deriving (Show, Eq)

type CxtId = String
type MacSig = [CxtId]
type MacParams = [Identifier]

data Node = SymNode Identifier
          | CharNode Char
          | NumNode  Double
          | ListNode [Node]
          | IfNode Node Node Node
          | LambdaNode Identifier Node
          | DefineNode Identifier Node
          | FuncallNode Node Node
          | PrintNode Node
          | ConsNode Node Node
          | CarNode Node
          | CdrNode Node
          | MaccallNode Node Node
          | MacroNode Node
          deriving (Eq)

instance Show Node where
  show (SymNode sym) = concat ["'", sym]
  show (CharNode c) = show c
  show (NumNode n) = show n
  show (ListNode l) = show l
  show (IfNode a b c) = concat ["!if", (indent2 $ show a), (indent2 $ show b), (indent2 $ show c)]
  show (LambdaNode a b) = concat ["!lambda", (indent2 $ show a), (indent2 $ show b)]
  show (DefineNode a b) = concat ["!define", (indent2 $ show a), (indent2 $ show b)]
  show (FuncallNode a b) = concat ["!funcall", (indent2 $ show a), (indent2 $ show b)]
  show (MaccallNode a b) = concat ["#maccall", (indent2 $ show a), (indent2 $ show b)]
  show (MacroNode a) = concat ["#{", (indent2 $ show a ++ "\n}")]
  show (PrintNode a) = concat ["!print", (indent2 $ show a)]
  show (ConsNode a b) = concat ["!cons", (indent2 $ show a), (indent2 $ show b)]
  show (CarNode a) = concat ["!car", show a]
  show (CdrNode a) = concat ["!cdr", show a]

indent :: String -> String -> String
indent idt node = foldl (\str x -> concat [str, "\n", idt,  x]) "" (lines node)
indent2 node = indent "  " node

parse :: FilePath -> String -> Either ParseError [ToplevelNode]
parse fname program =
      case P.parse parseProgram fname program of
            Left x -> Left x
            Right node -> Right node

parseMarkAsIdentifer :: Parser Identifier
parseMarkAsIdentifer = parseMarkAsIdentifer' <|> parseIdAsIdentifier
          where parseMarkAsIdentifer' = try $ do
                a <- beginLetter
                b <- many containLetter
                return (a:b)
                -- ruby -e 'puts [*33..47, *58..64, *91..96, *123..126].map(&:chr).join'
                where beginLetter = oneOf "!\"#$%&'()*+,-./;<=>?@[\\]^_`{|}~"
                      containLetter = beginLetter


parseIdAsIdentifier :: Parser Identifier
parseIdAsIdentifier = try parseSymIdAsIdentifier
                    where parseSymIdAsIdentifier = do
                                                 a <- beginLetter
                                                 b <- many containLetter
                                                 return (a:b)
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

parseString :: Parser Node
parseString = liftM ListNode (between (char '"') (char '"') (many parseChar)) <?> "a string"
            where parseChar :: Parser Node
                  parseChar = liftM CharNode (try (string "\\\"" >> return '"')
                                             <|> noneOf ['"'])

parseChar :: Parser Node
parseChar = liftM CharNode (prefix >> anyChar)
          where prefix = try $ string "$'"

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

parseProgram :: Parser [ToplevelNode]
parseProgram = do
             stats <- many $ parseEvalCxtStat <|> parseMacCxtStat
             eof
             return stats

parseMacCxtStat :: Parser ToplevelNode
parseMacCxtStat = parseMacDefTL
                where parseMacDefTL = try $ do
                                  macDef <- parseMacDef
                                  do { string ";"; return () } <|> do { eof; return () }
                                  skipSpaces
                                  return $ MacCxtTLNode macDef

parseMacSig :: Parser MacSig
parseMacSig = fnType <|> primType <?> "signature"
             where primType = try $ do
                            cxtId <- parseCxtId
                            return [cxtId]
                   fnType = try $ do
                          cxtId <- parseCxtId
                          requireSpaces
                          string "->"
                          requireSpaces
                          lst <- parseMacSig
                          return (cxtId:lst)

parseCxtId :: Parser CxtId
parseCxtId = try parseCxtId'
           where parseCxtId' = do
                             a <- beginLetter
                             b <- many containLetter
                             return $ (a:b)
                             where beginLetter = letter
                                   containLetter = letter <|>
                                                   oneOf "0123456789" <|>
                                                   oneOf "-"

parseMacDef :: Parser MacCxtNode
parseMacDef = parseMacDef2 <|> parseMacDef1 <?> "macro defination"
            where parseMacDef2 = try $ do
                               (id, params) <- parseMacDefIdAndParams
                               requireSpaces
                               string "::"
                               requireSpaces
                               sig <- parseMacSig
                               return $ MacDef2MNode id sig params
                  parseMacDef1 = try $ do
                               (id, params) <- parseMacDefIdAndParams
                               requireSpaces
                               string ":"
                               requireSpaces
                               sig <- parseMacSig
                               requireSpaces
                               string "="
                               requireSpaces
                               defi <- parseMaccall
                               return $ MacDef1MNode id sig params (MacroNode defi)

parseMacDefIdAndParams :: Parser (Identifier, MacParams)
parseMacDefIdAndParams = brackets <|> infixOp <|> prefixOp
                       where brackets = bracket "(" ")" <|>
                                        bracket "[" "]"
                             bracket beg end = try $ do
                                     string beg
                                     skipSpaces
                                     param <- parseIdAsIdentifier
                                     skipSpaces
                                     string end
                                     return (beg, [param])
                             suffixOp = try $ do
                                   params <- many1 parseIdAsIdentifier
                                   skipSpaces
                                   id <- string "@" >> parseMarkAsIdentifer
                                   return (id, params)
                             infixOp = try $ do
                                   param1 <- parseIdAsIdentifier
                                   skipSpaces
                                   id <- (try $ string ":" >>
                                                parseMarkAsIdentifer)
                                         <|> (try $ do
                                                  sym <- string "=>"
                                                  return sym)
                                         <|> (try $ do
                                                  sym <- (string ",")
                                                  return sym)
                                   skipSpaces
                                   param2 <- parseIdAsIdentifier
                                   params <- many (try $ requireSpaces >>
                                                         parseIdAsIdentifier)
                                   return (id, (param1:param2:params))
                             prefixOp = try $ do
                                    id <- parseIdAsIdentifier
                                    params <- many (try $ requireSpaces >>
                                                          parseIdAsIdentifier)
                                    return (id, params)

parseEvalCxtStat :: Parser ToplevelNode
parseEvalCxtStat = try $ do
                 expr <- parseMaccall
                 skipSpaces
                 do { string ";"; return () } <|> do { eof; return () }
                 skipSpaces
                 return $ EvalCxtTLNode expr

parseExpr :: Parser Node
parseExpr = parseDollarPref <?> "a expression"

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
                                       ; expr2 <- parseDollarPref
                                       ; return $ (\node -> op node expr2)
                                       }) <|> (try $ do {
                                         op <- suffixOp
                                         ; return op
                                       }))
                                 return $ foldl (\expr sfx -> sfx expr) expr1 sfxes

parseBracketMaccall :: Parser Node
parseBracketMaccall = parseBracket <|> parseVMInst <|> parseString <|> parseChar <|> parseId <|> parseNumber
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
                                        ; return $ foldl (\a b -> MaccallNode a b) (SymNode beg) (arg1:args)
                                    }
                          parseBracket = bracket "[" "]" <|>
                                       bracket "(" ")"
parseDollarPref = parseDollarPref' <|> parseLambdaSyntax
                <?> "dollar preferences"
                where parseDollarPref' = try $ do
                                       string "$"
                                       skipSpaces
                                       parseMaccall >>= return

parseLambdaSyntax :: Parser Node
parseLambdaSyntax = parseEqualArrow <|> parseComma <|> parseBracketMaccall

parseEqualArrow :: Parser Node
parseEqualArrow = try (do
                expr1 <- parseBracketMaccall
                skipSpaces
                string "=>"
                skipSpaces
                expr2 <- parseMaccall
                return $ MaccallNode (MaccallNode (SymNode "=>") expr1) expr2
                ) <?> "`=>'"

parseComma :: Parser Node
parseComma = try (do
           expr1 <- parseBracketMaccall
           skipSpaces
           string ","
           skipSpaces
           expr2 <- parseMaccall
           return $ MaccallNode (MaccallNode (SymNode ",") expr1) expr2
           ) <?> "`,'"

parseVMInst :: Parser Node
parseVMInst = parseVMIf <|> parseVMLambda <|> parseVMDefine <|> parseVMFuncall <|> parseVMPrint <|> parseVMCons <|> parseVMCar <|> parseVMCdr

parseVMIf :: Parser Node
parseVMIf = try $ do
          string "!if"
          requireSpaces
          cond <- parseExpr
          skipSpaces
          thenExpr <- parseExpr
          skipSpaces
          elseExpr <- parseExpr
          return $ IfNode cond thenExpr elseExpr

parseVMLambda :: Parser Node
parseVMLambda = try $ do
              string "!lambda"
              requireSpaces
              id <- parseIdAsIdentifier
              skipSpaces
              expr <- parseExpr
              return $ LambdaNode id expr

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

parseVMPrint :: Parser Node
parseVMPrint = try $ do
             string "!print"
             requireSpaces
             a <- parseExpr
             return (PrintNode a)

parseVMCons :: Parser Node
parseVMCons = try $ do
            string "!cons"
            requireSpaces
            a <- parseExpr
            requireSpaces
            b <- parseExpr
            return (ConsNode a b)

parseVMCar :: Parser Node
parseVMCar = try $ do
           string "!car"
           requireSpaces
           a <- parseExpr
           return (CarNode a)

parseVMCdr :: Parser Node
parseVMCdr = try $ do
           string "!cdr"
           requireSpaces
           a <- parseExpr
           return (CdrNode a)

skipSpaces = skipMany (oneOf " \t\n") <?> "skipped spaces"
requireSpaces = eof <|> (skipMany1 (oneOf " \t\n")) <?> "spaces"
