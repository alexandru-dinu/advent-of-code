module Solve where

import Data.Tree

data Expr
    = Regular Int
    | Pair Expr Expr
    deriving (Read, Eq)

instance Show Expr where
    show = drawTree . toStringTree

toStringTree :: Expr -> Tree String
toStringTree (Regular x) = Node (show x) []
toStringTree (Pair l r) = Node "o" [toStringTree l, toStringTree r]

t :: Expr
t =
    Pair
        (Pair
             (Regular 6)
             (Pair (Regular 5) (Pair (Regular 4) (Pair (Regular 3) (Regular 2)))))
        (Regular 1)

main :: IO ()
main = do
    xs <- lines <$> getContents
    let trees = map read xs :: [Expr]
    print $ head trees
