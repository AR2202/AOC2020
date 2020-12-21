module Day19
(testRule1,
testRule2,
testRule3,
testRule4,
testRule5,
testRule6,
testa,
testb,
testaorb,
testaorb',
testaandb,
testaandb',
testaandb'',
example19a,
solutionDay19a

)
where
import Common
import Data.List as L
import Data.IntMap.Strict as IM
import Data.List.Split(splitOn)
import Text.Parsec (ParseError,parse,eof,try,string)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Data.Char
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Either.Unwrap(fromRight,isRight)
import Data.Maybe(fromJust)


----------------------------------------
--Types
----------------------------------------

        
data Rule = Num Int
          | AndThen Rule Rule
          | OrElse Rule Rule
          | Str String
                  deriving (Eq,Show)

------------------
--Parser
------------------

term :: Parser Rule -> Parser Rule
term rule = numE <|> strE 

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t\""


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

numE :: Parser Rule
numE = do
    n <- lexeme $ many1 digit
    return $ Num $ read n

strE :: Parser Rule
strE = lexeme $ do
    fc <- firstChar
    rest <- many nonFirstChar
    return $ Str (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

parseA = string "a"
parseB = string "b"
--parseAthenB =  (++) <$> numE <*> strE

parseAorB p1 p2= try p1 <|> try p2

parseAthenB p1 p2 = do
    s1 <- p1
    s2 <- p2
    return (s1++s2)

parsechar :: String -> Parser String
parsechar x = string x

parseRule :: Parser Rule
parseRule = chainl1 term8 (try orelse <|> try andthen)
  where
    orelse = do
      
        void $ lexeme $ char '|'
       
        return OrElse
    andthen = do
        void $ many $ oneOf " \n\t"
        return AndThen
    term8 = term parseRule

parseRule' :: Parser Rule
parseRule' = do
   
    e <- term7
   
    maybeAddSuffix e
  where
   
    addSuffix e0 = do
        
        e1 <- term7
        maybeAddSuffix  (AndThen e0 e1)
    orelse e0 = do
        void $ lexeme $ char '|'
        e1 <- term7
        e2 <- maybeAddSuffix e1
        maybeAddSuffix (OrElse e0 e2)
    
    maybeAddSuffix' e = orelse e <|> addSuffix e <|> return e
    maybeAddSuffix e = addSuffix e <|> orelse e <|> return e
    

term7 :: Parser Rule
term7 = term parseRule'

testRule1 = parseWithWhitespace parseRule' "\"a\""
testRule2 = parseWithWhitespace parseRule' "1 2 "
testRule3 = parseWithWhitespace parseRule' "1 3 | 3 1"
testRule4 = parseWithWhitespace parseRule' "1  | 3 "
testRule5 = parseWithWhitespace parseRule' "1  | 3 1"
testRule6 = parseWithWhitespace parseRule' "1 3 | 1"
testa     = parseWithWhitespace parseA "a"
testb     = parseWithWhitespace parseB "b"
testaorb  = parseWithWhitespace (parseAorB parseA parseB) "a"
testaorb' = parseWithWhitespace (parseAorB parseA parseB) "b"
testaandb = parseWithWhitespace (parseAthenB parseA parseB) "ab"
testaandb' = parseWithWhitespace (parseAthenB parseA parseB) "ba"
testaandb'' = parseWithWhitespace (parseAthenB (parseAorB parseA parseB) parseB) "bb"

parseSnd :: (a,String) -> (a,  Rule)
parseSnd tuple = (fst tuple,fromRight $parseWithWhitespace parseRule' $snd tuple)

toTupleFirstToInt :: [String] -> (Int,String)
toTupleFirstToInt list = (read $ head list, last list)

lookupRule :: IntMap Rule -> Int -> Rule
lookupRule intmap key = eval $fromJust $ IM.lookup key intmap
    where eval  (Num i) = lookupRule intmap i
          eval  (OrElse x y) = OrElse (eval x) (eval y)
          eval  (AndThen x y) = AndThen (eval x) (eval y)
          eval  (Str s) = Str s

rule2parser (Str x) = parsechar x
rule2parser (AndThen x y) = parseAthenB (rule2parser x) (rule2parser y)
rule2parser (OrElse x y) = parseAorB (rule2parser x) (rule2parser y)

example19a :: IO ()
example19a = part1 "example19.txt"

solutionDay19a :: IO ()
solutionDay19a = part1 "input19.txt"

part1 :: String -> IO ()
part1 filename = do
    input <- splitOnBlankLine filename
    let ruleinput = L.map  (splitOn ":" ) $ lines $ head input
    let rules = L.map toTupleFirstToInt ruleinput
    let parsed = L.map parseSnd rules
    let messages = lines $ last input
    let rulemap = L.foldl' (flip (uncurry IM.insert)) empty parsed
    let evaluatedRules = lookupRule rulemap 0
    let parserFromRule = rule2parser evaluatedRules
    let parsedMessages = L.map (parseWithWhitespace parserFromRule) messages
    let result = (length . L.filter isRight) parsedMessages
    print result
    