import Data.Char

cwords :: Int -> FilePath -> FilePath -> IO()
cwords n infile outfile = do text <- readFile infile;
                                     writeFile outfile "aaa";
                                     putStrLn "cwords done!"

                                              
modernise :: String -> String
-- Below function should be point free! Get rid of title.             
modernise title = unwords . map (\(x:xs) -> toUpper x : xs ) . words $ title

type CIN = String
addSum :: CIN -> CIN
addSum nstr = nstr ++ chksum
              where chksum = show . sum $ map getDigit nstr
                     
getDigit :: Char -> Int
getDigit c = read [c] :: Int           

valid :: CIN -> Bool
valid s = s == addSum (take 8 s)

units = ["One","Two","Three","Four","Five","Six","Seven","Eight","Nine"]
tens = ["Ten","Twenty","Thirty","Fourty","Fifty","Sixty","Seventy","Eighty","Ninety"]
uniques = ["Eleven","Twelve","Thirteen","Fourteen","Fifteen","Sixteen","Seventeen","Eighteen","Nineteen"]

numstr n | n == 0 = ""
         | n < 10 = units !! (n - 1)
         | n `mod` 10 == 0 && n < 100 = tens !! (n `div` 10 - 1)
         | n < 20 = uniques !! (n `mod` 10 - 1)
         | n < 100 = tens !! (n `div` 10 - 1) ++ " " ++ units !! (n `mod` 10 - 1)
         | n < 1000 = units !! (n `div` 100 - 1) ++ " " ++ numstr (n `mod` 100) 


quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

sortedLetters = quicksort $ concat $ map (map toLower . numstr) [1 .. 999]               
letterCount = map (\x -> length $ filter (==x) sortedLetters) ['a' .. 'z']

palindrome s = s == reverse s
