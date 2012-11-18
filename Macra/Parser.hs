module Macra.Parser (runTimeExpr,
                     compileTimeExpr,
                     Identifier(..),
                     MacCxtNode(..),
                     Node(..),
                     CxtId,
                     MacSig,
                     MacParams) where

import Control.Monad
import Control.Applicative hiding ( (<|>)
                                  , many )
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
          | NativeNode Integer
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
  show (NativeNode a) = "!native " ++ (show a)

indent :: String -> String -> String
indent idt node = foldl (\str x -> concat [str, "\n", idt,  x]) "" (lines node)
indent2 node = indent "  " node


----------------------------------------
-- Compile Time Statements
----------------------------------------
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
                               defi <- semicolon
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
                                     param <- symbol
                                     skipSpaces
                                     string end
                                     return (beg, [param])
                             suffixOp = try $ do
                                   params <- many1 symbol
                                   skipSpaces
                                   id <- string "@" >> mark
                                   return (id, params)
                             infixOp = try $ do
                                   param1 <- symbol
                                   skipSpaces
                                   id <- (try $ string ":" >> mark)
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
                                   param2 <- symbol
                                   params <- many (try $ requireSpaces >> symbol)
                                   return (id, (param1:param2:params))
                             prefixOp = try $ do
                                     id <- symbol
                                     params <- many (try $ requireSpaces >> symbol)
                                     return (id, params)

----------------------------------------
-- Runtime Expression
----------------------------------------
runTimeExpr :: Parser Node
runTimeExpr = try (skipSpaces >> semicolon)

-- もっとも優先順位の低い中置関数。
-- a; b; c は a ; (b ; c) のように右に再帰する。
-- semicolon-expression:
--   funcall-expression; semicolon-expression
--   funcall-expression
semicolon :: Parser Node
semicolon = try semicolon' <|> funcall
          where semicolon' = pure (\expr1 sym -> FuncallNode (FuncallNode (SymNode sym) expr1))
                           <*> funcall
                           <*> (skipSpaces >> string ";")
                           <*> (skipSpaces >> semicolon)

-- funcall-expression:
--   funcall-expression arrow-expression
--   funcall-expression :identifier arrow-expression
--   funcall-expression @identifier
--   arrow-expression
funcall :: Parser Node
funcall = parseMaccall' <?> "one of prefix/infix/suffix"
             where maccall = infixOp <|> prefixOp
                   prefixOp = try $ do
                            requireSpaces
                            return FuncallNode
                   infixOp = try $ do
                           skipSpaces
                           string ":"
                           id <- pure SymNode <*> try mark
                           skipSpaces
                           return $ a (FuncallNode id)
                   a :: (Node -> Node) -> Node -> Node -> Node
                   a f n m = FuncallNode (f n) m
                   suffixOp = try $ do
                            skipSpaces
                            string "@"
                            id <- pure SymNode <*> try mark
                            return $ FuncallNode id
                   parseMaccall' = try $ do
                                 expr1 <- arrow
                                 sfxes <- many ((try $ do {
                                       op <- maccall
                                       ; expr2 <- arrow
                                       ; return $ (\node -> op node expr2)
                                       }) <|> (try $ do {
                                         op <- suffixOp
                                         ; return op
                                       }))
                                 return $ foldl (\expr sfx -> sfx expr) expr1 sfxes


-- arrow-expression:
--   primary-expression => funcall-expression
--   primary-expression , funcall-expression
--   primary-expression
arrow :: Parser Node
arrow = equalArrow <|> comma <|> prim
                  where equalArrow :: Parser Node
                        equalArrow = try $ pure (\expr1 sym -> FuncallNode (FuncallNode (SymNode sym) expr1))
                                           <*> prim
                                           <*> (skipSpaces >> string "=>")
                                           <*> (skipSpaces >> funcall)

                        comma :: Parser Node
                        comma = try $ pure (\expr1 sym -> FuncallNode (FuncallNode (SymNode sym) expr1))
                                      <*> prim
                                      <*> (skipSpaces >> string ",")
                                      <*> (skipSpaces >> funcall)


-- primary-expression:
--   [ semicolon-expression ]
--   { semicolon-expression }
--   ( semicolon-expression )
--   exclam-expression
--   identifier
--   constant
prim :: Parser Node
prim = brackets <|> exclamExpr <|> strLit <|> charLit <|> id <|> parseNumber
     where bracket beg end = try $ do { string beg
                                      ; skipSpaces
                                      ; expr <- semicolon
                                      ; skipSpaces
                                      ; string end
                                      ; return (FuncallNode (SymNode beg) expr)
                                      }
           brackets = bracket "[" "]" <|>
                      bracket "(" ")" <|>
                      bracket "{" "}"

           id :: Parser Node
           id = pure SymNode <*> try symbol

           strLit :: Parser Node
           strLit = do
                  str <- str'
                  return $ foldr (\ch str -> ConsNode ch str) NilNode str
                  where char' :: Parser Node
                        char' = liftM CharNode (try (string "\\\"" >> return '"')
                                             <|> noneOf ['"'])
                        str' = (between (char '"') (char '"') (many char')) <?> "a string"

           charLit :: Parser Node
           charLit = liftM CharNode (prefix >> anyChar)
                   where prefix = try $ string "$'"

mark :: Parser Identifier
mark = parseMarkAsIdentifer' <|> symbol
     where parseMarkAsIdentifer' = try $ do
           a <- beginLetter
           b <- many containLetter
           return (a:b)
           -- ruby -e 'puts [*33..47, *58..64, *91..96, *123..126].map(&:chr).join'
           where beginLetter = oneOf "!\"#$%&'()*+,-./;<=>?@[\\]^_`{|}~"
                 containLetter = beginLetter


symbol :: Parser Identifier
symbol = try parseSymIdAsIdentifier
       where parseSymIdAsIdentifier = do
                                    a <- beginLetter
                                    b <- many containLetter
                                    return (a:b)
                                    where beginLetter = letter
                                          containLetter = letter <|> oneOf "0123456789" <|> oneOf "-"

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

exclamExpr :: Parser Node
exclamExpr = try $ string "!" >> ( excIf <|> excLambda <|> excDefine <|>
                                   excFuncall <|> excPrint <|> excCons <|>
                                   excCar <|> excCdr <|> excDo <|> excNative )
            where excIf :: Parser Node
                  excIf = pure IfNode
                          <*> (try $ string "if" >> requireSpaces >> parseExpr)
                          <*> (skipSpaces >> parseExpr)
                          <*> (skipSpaces >> parseExpr)

                  excLambda :: Parser Node
                  excLambda = pure LambdaNode
                              <*> (try $ string "lambda" >> requireSpaces >> symbol)
                              <*> (skipSpaces >> parseExpr)

                  excDefine :: Parser Node
                  excDefine = pure DefineNode
                              <*> (try $ string "define" >> requireSpaces >> symbol)
                              <*> (skipSpaces >> parseExpr)

                  excFuncall :: Parser Node
                  excFuncall = pure FuncallNode
                               <*> (try $ string "funcall" >> requireSpaces >> parseExpr)
                               <*> (skipSpaces >> parseExpr)

                  excPrint :: Parser Node
                  excPrint = pure PrintNode
                             <*> (try $ string "print" >> requireSpaces >> parseExpr)

                  excCons :: Parser Node
                  excCons = pure ConsNode
                            <*> (try $ string "cons" >> requireSpaces >> parseExpr)
                            <*> (requireSpaces >> parseExpr)

                  excCar :: Parser Node
                  excCar = pure CarNode
                           <*> (try $ string "car" >> requireSpaces >> parseExpr)

                  excCdr :: Parser Node
                  excCdr = pure CdrNode
                           <*> (try $ string "cdr" >> requireSpaces >> parseExpr)

                  excDo :: Parser Node
                  excDo = pure DoNode
                          <*> (try $ string "do" >> requireSpaces >> parseExpr)
                          <*> (requireSpaces >> parseExpr)

                  -- nativeはIntのidで指定する
                  excNative :: Parser Node
                  excNative = pure NativeNode
                              <*> (try $ string "native" >> requireSpaces >> parseIntNum)

                  parseExpr :: Parser Node
                  parseExpr = arrow <?> "a expression"


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

spaces = oneOf " \t\n" >> return ()
skipCompileTimeExpr = try (string "#" >> parseMacDef >> return ())
skipSpaces = skipMany ( spaces <|> skipComment <|> skipCompileTimeExpr) <?> "skipped spaces"
requireSpaces = eof <|> (skipMany1 (spaces <|> skipComment <|> skipCompileTimeExpr)) <?> "spaces"
