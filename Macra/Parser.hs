module Macra.Parser (program,
                     compileTimeExpr,
                     Identifier(..),
                     MacCxtNode(..),
                     Node(..),
                     CxtId,
                     MacSig,
                     MacParams) where

import Control.Monad
import qualified Control.Applicative as A
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding (spaces)

-- シンボルの id
type Identifier = String

-- マクロ定義や、将来追加されるかもしれない
-- #include など、 `#' から始まるコンパイル時の命令
data MacCxtNode -- 普通のマクロ定義。
                -- #[ m a : t -> u = a ]
                = MacDef1MNode Identifier MacSig MacParams Node
                -- 関数に対するコンテキストの定義。
                -- # f :: t -> u
                | MacDef2MNode Identifier MacSig MacParams
                deriving (Show, Eq)

type CxtId = String              -- マクロのコンテキストのid
type MacSig = [CxtId]            -- マクロのシグネチャ
type MacParams = [Identifier]    -- マクロの仮引数

data Node = SymNode Identifier
          | CharNode Char
          | NumNode  Double
          | NilNode
          | IfNode Node Node Node
          | LambdaNode Identifier Node
          | DefineNode Identifier Node
          | FuncallNode Node Node
          | PrintNode Node
          | ConsNode Node Node
          | CarNode Node
          | CdrNode Node
          | DoNode Node Node
          -- macroExpand で展開済みのマクロを展開するのを防ぐノード
          -- macroExpand の実装では、
          --   #[ m a x : t -> t -> t = a ]
          --   m x w
          -- のように書くと、
          --   まず `a' が x に置換され、
          -- そのあと置換後の `x' が `w' に置換される。
          -- TODO: これは設計が汚いか？ 
          --       MacroNode を消してmacroExpandのほうでなんとかすべき。
          | MacroNode Node
          deriving (Eq)

instance Show Node where
  show NilNode = "nil"
  show (SymNode sym) = concat ["'", sym]
  show (CharNode c) = show c
  show (NumNode n) = show n
  show (IfNode a b c) = concat ["!if", (indent2 $ show a), (indent2 $ show b), (indent2 $ show c)]
  show (LambdaNode a b) = concat ["!lambda", (indent2 $ show a), (indent2 $ show b)]
  show (DefineNode a b) = concat ["!define", (indent2 $ show a), (indent2 $ show b)]
  show (FuncallNode a b) = concat ["!funcall", (indent2 $ show a), (indent2 $ show b)]
  show (MacroNode a) = concat ["#{", (indent2 $ show a ++ "\n}")]
  show (PrintNode a) = concat ["!print", (indent2 $ show a)]
  show (ConsNode a b) = concat ["!cons", (indent2 $ show a), (indent2 $ show b)]
  show (CarNode a) = concat ["!car", show a]
  show (CdrNode a) = concat ["!cdr", show a]
  show (DoNode a b) = concat ["!do", (indent2 $ show a), (indent2 $ show b)]

indent :: String -> String -> String
indent idt node = foldl (\str x -> concat [str, "\n", idt,  x]) "" (lines node)
indent2 node = indent "  " node

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
parseMark = A.pure SymNode A.<*> try parseMarkAsIdentifer

parseId :: Parser Node
parseId = A.pure SymNode A.<*> try parseIdAsIdentifier

parseString :: Parser Node
parseString = do
            str <- parseStr
            return $ foldr (\ch str -> ConsNode ch str) NilNode str
            where parseChar :: Parser Node
                  parseChar = liftM CharNode (try (string "\\\"" >> return '"')
                                             <|> noneOf ['"'])
                  parseStr = (between (char '"') (char '"') (many parseChar)) <?> "a string"

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

compileTimeExpr :: Parser [MacCxtNode]
compileTimeExpr = many $ try $ do
                    skipProgram
                    string "#"
                    parseMacDef
                 where skipProgram = skipMany $ noneOf "#"

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
                               skipSpaces
                               (id, params) <- parseMacDefIdAndParams
                               requireSpaces
                               string "::"
                               requireSpaces
                               sig <- parseMacSig
                               return $ MacDef2MNode id sig params
                  parseMacDef1 = try $ do
                               skipSpaces
                               string "["
                               skipSpaces
                               (id, params) <- parseMacDefIdAndParams
                               requireSpaces
                               string ":"
                               requireSpaces
                               sig <- parseMacSig
                               requireSpaces
                               string "="
                               requireSpaces
                               defi <- parseSemicolon
                               skipSpaces
                               string "]"
                               return $ MacDef1MNode id sig params (MacroNode defi)

parseMacDefIdAndParams :: Parser (Identifier, MacParams)
parseMacDefIdAndParams = brackets <|> infixOp <|> prefixOp
                       where brackets = bracket "(" ")" <|>
                                        bracket "[" "]" <|>
                                        bracket "{" "}"
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
                                         <|> (try $ do
                                                  sym <- (string ";")
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

program :: Parser Node
program = try (skipSpacesAndCompileTimeExpressions >> parseSemicolon)
        where skipSpacesAndCompileTimeExpressions = do
                skipSpaces
                try (string "#" >> parseMacDef >> skipSpacesAndCompileTimeExpressions)

parseExpr :: Parser Node
parseExpr = parseLambdaSyntax <?> "a expression"

parseSemicolon :: Parser Node
parseSemicolon = parseSemicolon' <|> parseMaccall
               where parseSemicolon' = try $ do
                                     expr1 <- parseMaccall
                                     skipSpaces
                                     string ";"
                                     skipSpaces
                                     expr2 <- parseSemicolon
                                     return (FuncallNode
                                              (FuncallNode
                                                (SymNode ";")
                                                expr1)
                                              expr2)

parseMaccall :: Parser Node
parseMaccall = parseMaccall' <?> "one of prefix/infix/suffix"
             where maccall = infixOp <|> prefixOp
                   prefixOp = try $ do
                            requireSpaces
                            return FuncallNode
                   infixOp = try $ do
                           skipSpaces
                           string ":"
                           id <- parseMark
                           skipSpaces
                           return $ a (FuncallNode id)
                   a :: (Node -> Node) -> Node -> Node -> Node
                   a f n m = FuncallNode (f n) m
                   suffixOp = try $ do
                            skipSpaces
                            string "@"
                            id <- parseMark
                            return $ FuncallNode id
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
parseBracketMaccall = parseBracket <|> parseVMInst <|> parseString <|> parseChar <|> parseId <|> parseNumber
                    where bracket beg end = try $ do {
                                        string beg
                                        ; skipSpaces
                                        ; expr <- parseSemicolon
                                        ; skipSpaces
                                        ; string end
                                        ; return (FuncallNode (SymNode beg) expr)
                                    }
                          parseBracket = bracket "[" "]" <|>
                                       bracket "(" ")" <|>
                                       bracket "{" "}"

parseLambdaSyntax :: Parser Node
parseLambdaSyntax = parseEqualArrow <|> parseComma <|> parseBracketMaccall

parseEqualArrow :: Parser Node
parseEqualArrow = try (do
                expr1 <- parseBracketMaccall
                skipSpaces
                string "=>"
                skipSpaces
                expr2 <- parseMaccall
                return $ FuncallNode (FuncallNode (SymNode "=>") expr1) expr2
                ) <?> "`=>'"

parseComma :: Parser Node
parseComma = try (do
           expr1 <- parseBracketMaccall
           skipSpaces
           string ","
           skipSpaces
           expr2 <- parseMaccall
           return $ FuncallNode (FuncallNode (SymNode ",") expr1) expr2
           ) <?> "`,'"

parseVMInst :: Parser Node
parseVMInst = parseVMIf <|> parseVMLambda <|> parseVMDefine <|> parseVMFuncall <|> parseVMPrint <|> parseVMCons <|> parseVMCar <|> parseVMCdr <|> parseVMDo

parseVMIf :: Parser Node
parseVMIf = A.pure IfNode
            A.<*> (try $ string "!if" >> requireSpaces >> parseExpr)
            A.<*> (skipSpaces >> parseExpr)
            A.<*> (skipSpaces >> parseExpr)

parseVMLambda :: Parser Node
parseVMLambda = A.pure LambdaNode
                A.<*> (try $ string "!lambda" >> requireSpaces >> parseIdAsIdentifier)
                A.<*> (skipSpaces >> parseExpr)

parseVMDefine :: Parser Node
parseVMDefine = A.pure DefineNode
                A.<*> (try $ string "!define" >> requireSpaces >> parseIdAsIdentifier)
                A.<*> (skipSpaces >> parseExpr)

parseVMFuncall :: Parser Node
parseVMFuncall = A.pure FuncallNode
                 A.<*> (try $ string "!funcall" >> requireSpaces >> parseExpr)
                 A.<*> (skipSpaces >> parseExpr)

parseVMPrint :: Parser Node
parseVMPrint = A.pure PrintNode
               A.<*> (try $ string "!print" >> requireSpaces >> parseExpr)

parseVMCons :: Parser Node
parseVMCons = A.pure ConsNode
              A.<*> (try $ string "!cons" >> requireSpaces >> parseExpr)
              A.<*> (requireSpaces >> parseExpr)

parseVMCar :: Parser Node
parseVMCar = A.pure CarNode
             A.<*> (try $ string "!car" >> requireSpaces >> parseExpr)

parseVMCdr :: Parser Node
parseVMCdr = A.pure CdrNode
             A.<*> (try $ string "!cdr" >> requireSpaces >> parseExpr)

parseVMDo = A.pure DoNode
            A.<*> (try $ string "!do" >> requireSpaces >> parseExpr)
            A.<*> (requireSpaces >> parseExpr)

skipComment :: Parser ()
skipComment = try $ do
            string "----"
            begMark <- many (char '-')
            skip ("----" ++ begMark)
            return ()
            where skip begMark = do
                       skipMany (noneOf "-")
                       endMark <- many1 (char '-')
                       if begMark == endMark
                         then return ()
                         else skip begMark

spaces = oneOf " \t\n"
skipSpaces = skipMany ( (spaces >> return ()) <|>
                        skipComment) <?> "skipped spaces"
requireSpaces = eof <|> (skipMany1 ((spaces >> return ()) <|>
                                     skipComment)) <?> "spaces"
