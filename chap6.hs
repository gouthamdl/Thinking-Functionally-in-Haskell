
-- Prelude definition for scanl
-- Below, by preprending the e at the start,we are keeping track of accumulated values
-- as scanl traverses the list. This keeping track is essentially how it
-- differs from fold since fold would only return the last value
scanl2 f e xs = e : (case xs of
                       [] -> []
                       x:xs -> scanl2 f (f e x) xs)
inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

mss :: [Int] -> Int
mss = maximum . map sum . segments

segments :: [Int] -> [[Int]]
segments = concat . map inits . tails

segments2 [] = [[]]
segments2 (x:xs) = inits (x:xs) ++ segments2 xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

mps = maximum . map sum . inits
