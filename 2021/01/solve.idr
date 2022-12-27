import Data.Vect

-- validConv : (Num t) => Vect (m + n) t -> Vect m t -> Vect (n + 1) t
-- validConv xs w = fromList [sum (window i (length w) xs) | i <- [0..length(xs)-length(w)]]

window : (i : Nat) -> (m : Nat) -> Vect (i + (m + n)) t -> Vect m t
window i m xs = take m (drop i xs)

-- xs : Vect 5 Int
-- xs = [1,2,3,4,5]

-- ys : List (Vect 3 Int)
-- ys = [window i 3 xs | i <- [1,3]]
