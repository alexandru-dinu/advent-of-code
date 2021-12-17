{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Data.Char

-- | A packet is either a V(alue) or an O(perator).
-- | They containt version, type, data.
data Packet
    = V Int Int Int      -- value
    | O Int Int [Packet] -- operator
    deriving (Show, Read)

data Parser a =
    Parser { parseWith :: String -> [(a, String)] }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> [(f a, rest) | (a, rest) <- p s]

instance Applicative Parser where
    pure x = Parser $ \s -> [(x, s)]
    (Parser p1) <*> (Parser p2) = Parser $ \s -> [(f x, r2) | (f, r1) <- p1 s, (x, r2) <- p2 r1]

instance Monad Parser where
    return = pure
    p >>= f = Parser $ \s -> concatMap (\(x, r) -> parseWith (f x) r) $ parseWith p s

instance Alternative Parser where
    empty = failure
    p1 <|> p2 =
        Parser $ \s ->
            case parseWith p1 s of
                [] -> parseWith p2 s
                res -> res

-- | Parsing utils
-- representation of a _failure_ parser
-- note that this is different from pure []
failure :: Parser a
failure = Parser $ \s -> []

-- consume a char (move _cursor_ one pos the right)
move :: Parser Char
move =
    Parser $ \case
        [] -> []
        (c:cs) -> [(c, cs)]

-- char parser - match a char c
charp :: Char -> Parser Char
charp c = fromPredicate (== c)

-- construct a char parser from a predicate
fromPredicate :: (Char -> Bool) -> Parser Char
fromPredicate pred =
    move >>=
    (\c ->
         if pred c
             then return c
             else failure)

-- (+) quantifier
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = p >>= (\v -> (zeroOrMore p) >>= (\v' -> return (v : v')))

-- (*) quantifier
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

exactly :: Int -> Parser a -> Parser [a]
exactly 0 _ = pure []
exactly n p = do
    v <- p
    v' <- exactly (n - 1) p
    return (v : v')

bin2int :: String -> Int
bin2int = foldl (\acc x -> 2 * acc + x) 0 . map digitToInt

takeintp :: Int -> Parser Int
takeintp n = exactly n move >>= (return . bin2int)

innerp :: Parser a -> String -> Parser [a]
innerp p s =
    case parseWith (oneOrMore p) s of
        [(arr, "")] -> return arr
        _ -> failure

-- | TODO: extremely ugly...
typep :: Int -> (String -> Bool) -> Parser Int
typep n pred = do
    s <- exactly n move
    if pred s
        then return (bin2int s)
        else failure

-- | Packet parsing
packetp :: Parser Packet
packetp = zerop <|> onep <|> valuep

valuep :: Parser Packet
valuep = do
    ver <- takeintp 3
    typ <- typep 3 (== "100")
    val <- valseqp
    return $ V ver typ val
  where
    -- (1[01]{4})* (0[01]{4})
    valseqp :: Parser Int
    valseqp = go >>= (return . bin2int)
      where
        go :: Parser String
        go = do
            c <- move
            v <- exactly 4 move
            v' <-
                if c == '0'
                    then pure []
                    else go
            return (v ++ v')

zerop :: Parser Packet
zerop = do
    ver  <- takeintp 3
    typ  <- typep 3 (/= "100")
    _    <- charp '0'
    len  <- takeintp 15
    ss   <- exactly len move
    kids <- innerp packetp ss
    return $ O ver typ kids

onep :: Parser Packet
onep = do
    ver  <- takeintp 3
    typ  <- typep 3 (/= "100")
    _    <- charp '1'
    len  <- takeintp 11
    kids <- exactly len packetp
    return $ O ver typ kids
