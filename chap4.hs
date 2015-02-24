divisors n = [d | d <- [2 .. n], n `mod` d == 0]
coprime x y = disjoint (divisors x) (divisors y)
disjoint [] ys = True
disjoint (x:xs) ys | isPresent x ys = False
                   | otherwise = disjoint xs ys
isPresent x [] = False
isPresent x (y:ys) | x > y = isPresent x ys
                   | x == y = True
                   | x < y = False
ramanujan = [(a^3 + b^3,a,b,c,d) | a <- [1 .. 100], b<- [a .. 100], c <- [a+1 ..100],a /= c, d <- [c .. 100], a^3 + b^3 == c^3 + d^3]


data List a = Nil | Snoc (List a) a deriving Show
head' Nil = undefined
head' (Snoc Nil x) = x
head' (Snoc xs x) = head' xs

tail' Nil = Nil
tail' (Snoc Nil x) = Nil
tail' (Snoc xs x) = Snoc (tail' xs) x            

toList xs = toList' (reverse xs)
toList' [] = Nil            
toList' (x:xs) = Snoc (toList' xs) x

fromList' Nil = []
fromList' (Snoc xs x) = x : fromList' xs
fromList = reverse . fromList'                        

take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

splitAt' n [] = ([],[])
splitAt' n (x:xs) | n == 0 = ([], x:xs)
                 | otherwise = (x:ys,zs)
                               where (ys,zs) = splitAt' (n-1) xs
