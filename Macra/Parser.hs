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
                    macDef
                 where skipProgram = skipMany $ noneOf "#"

macDef :: Parser MacCxtNode
macDef = macDef2 <|> macDef1 <?> "macro defination"
       where macDef2 = try $ pure (\(id, params) sig -> MacDef2MNode id sig params)
                             <*> ( skipSpaces >> idAndParams)
                             <*> ( requireSpaces >> string "::"
                                >> requireSpaces >> macSig )
             macDef1 = try $ pure (\(id, params) sig defi end -> MacDef1MNode id sig params (MacroNode defi))
                             <*> ( skipSpaces >> string "["
                                >> skipSpaces >> idAndParams )
                             <*> ( requireSpaces >> string ":"
                                >> requireSpaces >> macSig )
                             <*> ( requireSpaces >> string "="
                                >> requireSpaces >> semicolon )
                             <*> (skipSpaces >> string "]")

             macSig :: Parser MacSig
             macSig = fnType <|> primType <?> "signature"
                    where primType = try $ do
                                     cxt <- cxtId
                                     return [cxt]
                          fnType = try $ do
                                 cxt <- cxtId
                                 requireSpaces
                                 string "->"
                                 requireSpaces
                                 lst <- macSig
                                 return (cxt:lst)

             cxtId :: Parser CxtId
             cxtId = symbol

             idAndParams :: Parser (Identifier, MacParams)
             idAndParams = brackets <|> infixMacDef <|> prefixMacDef <|> suffixMacDef
                         where brackets = bracket "(" ")" <|>
                                          bracket "[" "]" <|>
                                          bracket "{" "}"
                               bracket beg end = try $ pure (\beg param end -> (beg, [param]))
                                                       <*> (string beg)
                                                       <*> (skipSpaces >> symbol)
                                                       <*> (skipSpaces >> (string end))
                               infixOpList = [ string ":" >> mark
                                             , string "=>"
                                             , string "->"
                                             , string ","
                                             , string ";"
                                             ]
                               infixOp = foldl (\x y -> x <|> y)
                                               (head infixOpList)
                                               (tail infixOpList)
                               infixMacDef = try $ pure (\param1 id param2 params -> (id, (param1:param2:params)))
                                                   <*> symbol
                                                   <*> (skipSpaces >> infixOp)
                                                   <*> (skipSpaces >> symbol)
                                                   <*> (many (try $ requireSpaces >> symbol))
                               prefixMacDef = try $ pure (\id params -> (id, params))
                                                    <*> symbol
                                                    <*> many (try $ requireSpaces >> symbol)
                               suffixMacDef = try $ pure (\params id -> (id, params))
                                                    <*> many1 symbol
                                                    <*> (skipSpaces >> string "@" >> mark)

----------------------------------------
-- Runtime Expression
----------------------------------------
runTimeExpr :: Parser Node
runTimeExpr = do { skipSpaces
                 ; expr <- semicolon
                 ; skipSpaces
                 ; eof
                 ; return expr
                 } <?> "a program containing at least one expression."

-- もっとも優先順位の低い中置関数。
-- a; b; c は a ; (b ; c) のように右に再帰する。
-- semicolon-expression:
--   funcall-expression; semicolon-expression
--   funcall-expression
semicolon :: Parser Node
semicolon = try semicolon' <|> funcall <?> "semicolon-expression"
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
funcall = funcall' <?> "funcall-expression"
        where prefixOp = skipSpaces >> return FuncallNode
              infixOp = try $ pure (\id a -> FuncallNode (FuncallNode (SymNode id) a))
                              <*> (skipSpaces >> string ":" >> mark)
              suffixOp = try $ pure (\id -> FuncallNode (SymNode id))
                               <*> (skipSpaces >> string "@" >> mark)
              funcall' = try $ do
                       expr1 <- arrow
                       sfxes <- many ((try $ pure (\op expr2 node -> op node expr2)
                                             <*> (infixOp <|> prefixOp)
                                             <*> (skipSpaces >> arrow))
                                  <|> (try suffixOp))
                       return $ foldl (\expr sfx -> sfx expr) expr1 sfxes


-- arrow-expression:
--   primary-expression => funcall-expression
--   primary-expression -> funcall-expression
--   primary-expression , funcall-expression
--   primary-expression
arrow :: Parser Node
arrow = arrow' <|> prim <?> "arrow-expression"
      where arrow' = try $ pure (\expr1 sym -> FuncallNode (FuncallNode (SymNode sym) expr1))
                           <*> prim
                           <*> (skipSpaces >> arrowMark)
                           <*> (skipSpaces >> funcall)

            arrowMark = foldl (\x mark -> x <|> (try $ string mark))
                              (try $ string $ head arrowList)
                              (tail arrowList)

            arrowList = [ "=>"
                        , "->"
                        , ","
                        ]


-- primary-expression:
--   [ semicolon-expression ]
--   { semicolon-expression }
--   ( semicolon-expression )
--   exclam-expression
--   identifier
--   constant
prim :: Parser Node
prim = brackets <|> exclamExpr <|> strLit <|> charLit <|> id <|> num
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

           num :: Parser Node
           num = floatNum <?> "a number"

           floatNum = try $ do
                    i <- int
                    ds <- (char '.' >> many1 digit) <|> return "0"
                    return $ NumNode (read $ concat [show i, ".", ds])

           int :: Parser Integer
           int = nonZero <|> zero <?> "a integer"
               where zero = try $ do { char '0'; return 0 }
                     nonZero = try $ do
                             sign <- char '-' <|> do {return ' '}
                             d <- beginDigit
                             ds <- many digit
                             return $ read $ concat [[sign], [d], ds]
                             where beginDigit = oneOf "123456789"

-- hoge :<> fuga とかの構文で使える記号の id
--   使える記号はまだ仕様が曖昧なので
--   ruby -e 'puts [*33..47, *58..64, *91..96, *123..126].map(&:chr).join'
-- で出力したものを使えるようにしてる。
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
symbol = symbol' <?> "symbol"
       where symbol'       = try $ do { beg <- beginLetter
                                      ; end <- symbolEnd
                                      ; return (beg:end)
                                      }
             beginLetter   = letter <|> oneOf "_"             -- シンボルの開始として許される文字。 abc の a
             containLetter = letter <|> digit <|> oneOf "-_"  -- シンボルに含める文字。 abc の b
             endLetter     = letter <|> digit <|> oneOf "_"   -- シンボルの終わりに含める文字。 abc の c
             symbolEnd     = symbolEnd1 <|> return []
             symbolEnd1    = (try $ do { lett <- containLetter
                                       ; last <- symbolEnd1
                                       ; return (lett:last)
                                       })
                             <|> try (pure (\lett -> [lett]) <*> endLetter)

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
                              <*> (try $ string "native" >> requireSpaces >> nativeId)
                            where idList = ["1001", "1002"]
                                  accept id = try $ pure read <*> string id
                                  nativeId = foldl (\x id -> x <|> accept id)
                                                   (accept $ head idList)
                                                   (tail idList) <?> "native id"


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
skipCompileTimeExpr = try (string "#" >> macDef >> return ())
skipSpaces = skipMany ( spaces <|> skipComment <|> skipCompileTimeExpr) <?> "skipped spaces"
requireSpaces = eof <|> (skipMany1 (spaces <|> skipComment <|> skipCompileTimeExpr)) <?> "spaces"
