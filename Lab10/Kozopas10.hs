{-# OPTIONS_GHC -Wall #-}
module Kozopas10 where

import Text.ParserCombinators.Parsec

-- Задача 1 -----------------------------------------
     -- <opMul>  ::= '*'  | '/'  | '%'
     -- opAdd>  ::= '+'  | '-'
     -- <digit>      ::=  ‘0’ | ‘1’ | ‘2’ | ‘3’ | ‘4’ | ‘5’ | ‘6’ | ‘7’ | ‘8’ | ‘9’ 
     -- <integer>  ::=  <digit> {<digit>}
     -- <factor>    ::= '('  <expr> ')' | <integer> 
     -- <term>      ::=  <factor> { <opMul> <factor> }
     -- <expr>      ::=  [<opAdd>] <term> { <opAdd> <term> }
     -- <evExpr>  ::=  <expr> 'eos'


data Expr1 = Add1 Expr1 Expr1 | Mul1 Expr1 Expr1 | Sub1 Expr1 Expr1 
             | Div1 Expr1 Expr1 | Mod1 Expr1 Expr1 | Lit1 Int
            deriving Show

sign :: Parser String 
sign = string "-" <|> pure ""

paren :: Parser a -> Parser a 
paren p = do _ <- string "("
             n <- p 
             _ <- string ")" 
             return n              
              
numb :: Parser Int
numb = do s <- sign 
          cs <- many1 digit
          return $ read (s ++ cs) 

int :: Parser Expr1
int = do { n <- numb; return (Lit1 n)}

infOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infOp x f = do _ <- string x
               return f

addop, mulop :: Parser (Expr1 -> Expr1 -> Expr1)
addop = (infOp "+" Add1) <|> (infOp "-" Sub1)
mulop = (infOp "*" Mul1) <|> (infOp "/" Div1) <|> (infOp "%" Mod1)

expr, term, factor :: Parser Expr1
expr   = term `chainl1` addop
term   = factor `chainl1` mulop
factor = int <|> paren expr     

eval :: Expr1 -> Int
eval ex = case ex of
  Add1 a b -> eval a + eval b
  Mul1 a b -> eval a * eval b
  Sub1 a b -> eval a - eval b
  Div1 a b -> eval a `div` eval b
  Mod1 a b -> eval a `mod` eval b
  Lit1 n   -> n            

exprAll :: Parser Expr1 
exprAll = do ex <- expr 
             eof 
             return ex   

evExpr  :: String -> Maybe Integer
evExpr st = case parse exprAll "" st of 
               Right ex -> Just (fromIntegral (eval ex))
               Left  _ -> Nothing

-- Задача 2 -----------------------------------------

     -- <digit>      ::=  ‘0’ | ‘1’ | ‘2’ | ‘3’ | ‘4’ | ‘5’ | ‘6’ | ‘7’ | ‘8’ | ‘9’ 
     -- <letter>    ::=  ‘a’| … \ ‘w’ | ‘A’ | … | ‘W’ 
     -- <spaces>     ::= { ‘ ‘ | ‘\t’ | ‘\r’ | ‘\n’}
     -- <integer>  ::=  <digit> {<digit>}
     -- <iden>      ::=  <letter> {<digit> | <letter> }
     --  <opMul>  ::= '*'  <spaces> | '/'  <spaces> | '%'<spaces>
     --  <opAdd>  ::= '+' <spaces> | '-'<spaces>
     -- <factorT>   ::= '(' <spaces> <expr> ')' <spaces>
     --                            | <integer> <spaces> | <iden> <spaces>
     -- <termT>     ::=  <factorT> { <opMul> <factorT> }
     -- <exprT>     ::=  [<opAdd>] <termT> { <opAdd> <termT> }
     -- <fullExpr> ::=  <spaces><exprT> 'eos'


data Expr = Add Expr Expr | Sub Expr Expr
          | Mul Expr Expr | Mod Expr Expr | Div Expr Expr
          | Var String | Lit Int
            deriving (Show, Eq)

number :: Parser Int
number = do cs <- many1 digit
            return $ read cs 

lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces ; return a}

reserved :: String -> Parser ()
reserved s = do { _ <- string s; spaces} 

parens :: Parser a -> Parser a 
parens p = do reserved "(" 
              n <- lexem p 
              reserved ")" 
              return n   

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

integer :: Parser Expr
integer = do { n <-  lexem number; return (Lit n)}

variable :: Parser String
variable = do s <- letter
              cs <- many alphaNum
              return ([s] ++ cs) 
               

iden :: Parser Expr
iden = do { n <- lexem variable; return (Var n)}       


opAdd, opMul :: Parser (Expr -> Expr -> Expr)
opAdd = (infixOp "+" Add) <|> (infixOp "-" Sub)
opMul = (infixOp "*" Mul) <|> (infixOp "/" Div) <|> (infixOp "%" Mod)


exprT, termT, factorT :: Parser Expr
exprT = termT `chainl1` opAdd
termT = factorT `chainl1` opMul
factorT = parens exprT <|> integer <|> iden

fullExpr :: Parser Expr
fullExpr = do spaces;
              ex <- exprT
              eof
              return ex

astExpr :: String -> Maybe Expr
astExpr str = case (parse fullExpr "" str) of
               Left _     -> Nothing
               Right ex -> Just ex

-- Задача 3 -----------------------------------------

   -- < reg>    :: =  <rexpr>  ‘eos’ 
   -- < rexpr> :: = <rterm> { '|'  <rterm>}
   -- < rterm> :: =  <rfact>  {<rfact>}
   -- < rfact>  :: =  <prime> {'*' | '+' | '?'} 
   -- < prime> ::=  <rsymb> | '(' <rexpr> ')'
   -- < rsymb> :: =<довільний символ крім  ‘(‘, ‘)’, ‘|’, ‘*’, ‘+’, ‘?’>  


data RE = Null |
          Term Char  | -- Термінальний символ
          Seq RE RE | -- Послідовність
          Alt RE RE  | -- Альтернатива
          Rep RE       | -- Повторення (*)
          Plus RE      | -- Повторення (+)
          Opt RE        -- Необов’язкове входження (?)
       deriving (Eq, Show) 

postfixOp :: String -> (a -> a) -> Parser (a -> a)
postfixOp x f = do _ <- string x
                   return f

altOp, seqOp :: Parser (RE -> RE -> RE)
altOp = infixOp "|" Alt
seqOp = infixOp "" Seq

repOp, optOp, plusOp :: Parser (RE -> RE)
repOp = postfixOp "*" Rep
optOp = postfixOp "?" Opt
plusOp = postfixOp "+" Plus

rsymb :: Parser RE
rsymb = do n <- noneOf ['(', ')', '|', '*', '+', '?']
           return (Term n)

prime :: Parser RE
prime = rsymb <|> paren rexpr

postOperations :: Parser (RE -> RE)
postOperations = optOp <|> repOp <|> plusOp

rfact :: Parser RE
rfact = do pr <- prime
           opt <- optionMaybe postOperations
           case opt of
               Nothing -> return pr
               Just op -> return $ op pr

rterm :: Parser RE
rterm = rfact `chainl1` seqOp

rexpr :: Parser RE
rexpr = rterm `chainl1` altOp

reg :: Parser RE
reg = do ex <- rexpr
         eof
         return ex

regExp :: String -> Maybe RE
regExp str = case (parse reg "" str) of
               Left _   -> Nothing
               Right rg -> Just rg

-- Задача 4 -----------------------------------------			   
    -- < wSp>           :: = {символи, що задовольняють isSpace}
    -- < textXML>   ::= {довільні символи крім  ‘<’ і ‘>’ }
    -- <valueXML> :: = {довільні символи крім  ‘”’ }
    -- <nameXML> ::=  <letter> {<letter> | <digit> | ‘.’ | ‘-‘}
    -- <element>     ::=‘<’ <nameXML> {<attribute>} ’>’ { <xmlXML> } ‘<’’/' <nameXML> ‘>’
    -- <attribute>     :: =  <wSp> <nameXML> <wSp> ‘=’  <wSp> ‘”’ <valueXML> ‘”’
    -- < xmlXML>  ::=  <element>  |  <textXML> 
    -- <fullXML>   ::=  <wSp> <element>  <wSp>  <eos>

type Name = String
type Attributes = [(String, String)]
data XML  =  Text String | Element Name Attributes [XML]
          deriving (Eq, Show)


wSp :: Parser String
wSp = many space

textXML :: Parser XML
textXML = do tx <- many (noneOf ['<', '>'])
             return (Text tx)

valueXML :: Parser String
valueXML = many (noneOf ['"'])

nameXML :: Parser String
nameXML = do lt <- letter
             rest <- many (letter <|> digit <|> oneOf ['.', '-'])
             return ([lt] ++ rest)


element :: Parser XML
element = do _ <- char '<'
             name <- nameXML
             attr <- many attribute
             _ <- char '>'
             xmls <- many (try xmlXML)
             _ <- char '<'
             _ <- char '/'
             _ <- nameXML
             _ <- char '>'
             return (Element name attr xmls)


attribute :: Parser (String, String)
attribute = do _ <- wSp
               name <- nameXML
               _ <- wSp
               _ <- char '='
               _ <- wSp
               _ <- char '"'
               val <- valueXML
               _ <- char '"'
               return (name, val)

xmlXML :: Parser XML
xmlXML = element <|> textXML

fullXML  :: Parser XML 
fullXML = do _ <- wSp
             ex <- element
             _ <- wSp
             eof
             return ex

anXML :: String -> Maybe XML
anXML str = case (parse fullXML "" str) of
               Left _    -> Nothing
               Right xml -> Just xml
------------------------------------------------------
re1, re2, re3, re4, re5 :: RE
re1  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2  = Seq (Term 'x') (Rep (Term '\''))
re3  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4  = Seq (Alt (Term 'a') Null) (Term 'a')
re5  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
      \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]