

cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

group [] = []
group xs = take 3 xs : group (drop 3 xs)

ungroup = concat           

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup .
       map cols .
       group . map group

