{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (fail)
import Data.Char

newtype Parser a = Parser (String -> [(a,String)])
apply (Parser p) = p
parse p = fst . head . apply p

instance Monad Parser where
    return a = Parser (\s -> [(a,s)])
    p >>= f = Parser (\s -> [(y,s'')
                            | (x,s') <- apply p s,
                              (y,s'') <- apply (f x) s'])

getc = Parser f
       where f [] = []
             f (x:xs) = [(x,xs)]

fail = Parser (const [])

guard True = return ()
guard False = fail
                        
sat p = do
  x <- getc
  guard (p x)
  return x

char x = sat (== x)

string [] = return ""
string (x:xs) = do
  char x
  string xs
  return (x:xs)

lower = sat isLower
digit = do
  d <- sat isDigit
  return (cvt d) where cvt d = fromEnum d - fromEnum '0'

p <|> q = Parser f
          where f s = let ps = apply p s in
                      if null ps then apply q s else ps

lowers = do {c <- lower; cs <- lowers; return(c:cs)} <|> return ""

-- The return is there at the end because if it wasn't,
-- xs <- many p would fail for anything other than a perfect parse
-- and so we will only see an empty list rather than how much of the string could be parsed
some p = do
          x <- p
          xs <- many p
          return (x:xs)

many p = some p <|> return []                 

space = many (sat isSpace) >> return ()       

symbol xs = space >> string xs

token p = space >> p 

natural :: Parser Int          
natural = token nat
nat = do
  ds <- some digit
  return (foldl1 shiftl ds)
      where shiftl m n = 10*m + n

int = do
  space
  f <- minus
  n <- nat
  return (f n)
      where minus = (char '-' >> return negate) <|> (char '+' >> return id) <|> return id
      
ints = bracket p <|> p
       where p = manywith (symbol ",") int

bracket p = do
  symbol "["
  x <- p
  symbol "]"
  return x

manywith q p = somewith q p <|> return []
somewith q p = do
  x <- p
  xs <- many (q >> p)
  return (x:xs)

data Expr = Con Int | Bin Op Expr Expr deriving Show
data Op = Plus | Minus |Mul | Div deriving Show

-- expr = token (constant <|> paren binary)
-- binary = do
--   e1 <- expr
--   p <- op
--   e2 <- expr
--   return (Bin p e1 e2)

paren p = do
  char '('
  x <- p
  char ')'
  return x

constant = do
  n <- nat
  return (Con n)  

expr = token (term >>= rest)
term = token (factor >>= more)
rest e1 = do
  p <- addOp
  e2 <- term
  rest (Bin p e1 e2)
          <|> return e1

more e1 = do
  p <- mulOp
  e2 <- factor
  more (Bin p e1 e2)
          <|> return e1

factor = token (constant <|> paren expr)

addOp = (symbol "+" >> return Plus) <|>
        (symbol "-" >> return Minus)         

mulOp = (symbol "*" >> return Plus) <|>
        (symbol "/" >> return Minus)

digit2 = sat isDigit
            
floats = token float
float = do
  x <- (zero <|> digit2)
  char '.'
  z <- (many digit2)
  return (x:'.':z)

zero = char '0'  




