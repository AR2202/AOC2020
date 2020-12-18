module Day18 (

testsimpleE4,
testsimpleE5,
testsimpleE6,
testsimpleE7,
testsimpleE8,
testsimpleE9,
testsimpleE10,
testsimpleE11,
testevalE9,
testevalE8,
testevalE7,
testevalsimpleE9,

testsequence,
solutionDay18a,
solutionDay18b
)
where

import Common
import Text.Parsec (ParseError,parse,eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar,oneOf, char, digit, satisfy)
import Text.Parsec.Char
import Data.Char
import Text.Parsec.Combinator (many1, choice, chainl1)
import Text.Parsec (try)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

----------------------------------------
--Types
----------------------------------------


        
data SimpleExpr = Num Integer
                | Add SimpleExpr SimpleExpr
                | Mult SimpleExpr SimpleExpr
                | Parens SimpleExpr
                  deriving (Eq,Show)

---------------------------------------
--Parsing
---------------------------------------

term :: Parser SimpleExpr -> Parser SimpleExpr
term simpleExprImpl = numE <|>  parensEN simpleExprImpl

parensEN :: Parser SimpleExpr -> Parser SimpleExpr
parensEN simpleExprImpl = do
    void $ lexeme $ char '('
    e <- simpleExprImpl
    void $ lexeme $ char ')'
    return $ Parens e

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"


parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
  where
    wrapper = do
        whitespace
        p

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

numE :: Parser SimpleExpr
numE = do
    n <- lexeme $ many1 digit
    return $ Num $ read n

addE :: Parser SimpleExpr
addE = do
    e0 <- numE
    void $ lexeme $ char '+'
    e1 <- numE
    return $ Add e0 e1

multE :: Parser SimpleExpr
multE = do
    e0 <- numE
    void $ lexeme $ char '+'
    e1 <- numE
    return $ Add e0 e1

simpleExprPart1 :: Parser SimpleExpr
simpleExprPart1 = chainl1  term8  (try multiplication <|> try addition)
  where
    addition = do
        void $ lexeme $ char '+'
        return Add
    multiplication = do
        void $ lexeme $ char '*'
        return Mult
    term8 = term simpleExprPart1





---------------------------
--evaluating expressions
---------------------------

eval :: SimpleExpr -> Integer
eval (Num x)      = x
eval (Add e1 e2)  = eval e1 + eval e2
eval (Mult e1 e2) = eval e1 * eval e2
eval (Parens e)   = eval e


----------------------
--Some tests
---------------------


testsimpleE4 = parseWithWhitespace simpleExprPart1 "1+2"
testsimpleE5 = parseWithWhitespace simpleExprPart1 "(1+2)"
testsimpleE6 = parseWithWhitespace simpleExprPart1 "1+ (2+3)"
testsimpleE7 = parseWithWhitespace simpleExprPart1 "1* (2+3)"
testsimpleE8 = parseWithWhitespace simpleExprPart1 "1+ 2*3"
testsimpleE9 = parseWithWhitespace simpleExprPart1 "1* 2+3"
testevalE9 = eval (Add (Mult (Num 1) (Num 2)) (Num 3))
testevalE7 = eval (Mult (Num 1) (Parens (Add (Num 2) (Num 3))))
testevalE8 = eval (Mult (Add (Num 1) (Num 2)) (Num 3))

testevalsimpleE9 = fmap eval testsimpleE9
testsequence = sequence [testsimpleE8,testsimpleE9]

------------------
--Part1
----------------

solutionDay18a :: IO ()
solutionDay18a = do
    lines <-loadAndSplitLines "input18.txt"
    let mathsproblems = map (parseWithWhitespace simpleExprPart1) lines
    let mathresults = map (fmap eval) mathsproblems
    let sumResults = fmap sum $ sequence mathresults
    print sumResults

------------------
--Part2
----------------

simpleExprPart2 :: Parser SimpleExpr
simpleExprPart2 = chainl1  (chainl1 term8  addition) multiplication
  where
    addition = do
        void $ lexeme $ char '+'
        return Add
    multiplication = do
        void $ lexeme $ char '*'
        return Mult
    term8 = term simpleExprPart2

testsimpleE10 = parseWithWhitespace simpleExprPart2 "1+ 2*3"
testsimpleE11 = parseWithWhitespace simpleExprPart2 "1* 2+3"

solutionDay18b :: IO ()
solutionDay18b = do
    lines <-loadAndSplitLines "input18.txt"
    let mathsproblems = map (parseWithWhitespace simpleExprPart2) lines
    let mathresults = map (fmap eval) mathsproblems
    let sumResults = fmap sum $ sequence mathresults
    print sumResults