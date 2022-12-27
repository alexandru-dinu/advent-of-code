-- Try to learn Parsec
-- References
-- https://www.rosettacode.org/wiki/Arithmetic_evaluation
-- https://jakewheat.github.io/intro_to_parsing/
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec-Expr.html

{-# LANGUAGE FlexibleContexts #-}

import System.Environment (getArgs)
import Text.Printf

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as E
import Text.Parsec.String (Parser)
import Control.Applicative
import Control.Monad (void)

data Expr = Num Int | Add Expr Expr | Mul Expr Expr deriving Show


eval :: Expr -> Int
eval (Num v) = v
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r


parseWhitespace :: Parser ()
parseWhitespace = void $ P.many $ P.oneOf " \t" -- matches ([ ]*)

-- Parser a -> Parser () -> Parser a
trim :: Parser a -> Parser a
trim p = p <* parseWhitespace

symbol :: String -> Parser String
symbol = trim . P.string

-- The nice part of using buildExpressionParser is that
-- we can specify custom operator precedence in `table`
parseExpr table = E.buildExpressionParser table factor
    where
        factor = parseParens <|> parseNum
        parseNum = (Num . read) <$> trim (P.many1 P.digit)
        parseParens = P.between (symbol "(") (symbol ")") (parseExpr table)

-- Normal operator precedence
normPrec = [ [E.Infix (Mul <$ symbol "*") E.AssocLeft]
           , [E.Infix (Add <$ symbol "+") E.AssocLeft]
           ]

-- prec(+) == prec(*)
samePrec = [ [ E.Infix (Add <$ symbol "+") E.AssocLeft
           , E.Infix (Mul <$ symbol "*") E.AssocLeft
           ] ]

-- prec(+) > prec(*)
diffPrec = [ [E.Infix (Add <$ symbol "+") E.AssocLeft]
           , [E.Infix (Mul <$ symbol "*") E.AssocLeft]
           ]


parseval table str =
    case P.parse (parseExpr table) "(src)" str of
        Right expr -> eval expr
        Left err -> error ("Pasing error!" ++ show err)


main :: IO ()
main = do
    args <- getArgs -- IO [String]
    contents <- readFile (head args) -- IO String
    let sol1 = map (parseval samePrec) (lines contents)
    let sol2 = map (parseval diffPrec) (lines contents)
    printf "Part 1: %d\n" $ sum sol1
    printf "Part 2: %d\n" $ sum sol2