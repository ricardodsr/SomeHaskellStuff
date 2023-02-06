module redofunct where
    import Data.Char


isSpace' :: Char -> Bool
 isSpace' c = if (c==' ') then True else False

 isUpper' :: Char -> Bool
 isUpper' c = (c >= 'A') && (c <= 'Z')

 isAlpha' :: Char -> Bool
 isAlpha' c = (isUpper c) || (isLower c)

 toUpper' :: Char -> Char
 toUpper' c = if (isLower c)
              then chr ((ord 'A')+(ord c)-(ord 'a'))
              else c

 digitToInt' :: Char -> Int
 digitToInt' c | isDigit c = (ord c) - (ord '0')
               | (c >= 'a') && (c<='f') = (ord c) - (ord 'a') + 10
               | (c >= 'A') && (c<='F') = (ord c) - (ord 'A') + 10
               | otherwise = error "not a digit."

 intToDigit' :: Int -> Char
 intToDigit' n | (n>=0) && (n<=9) = chr (n + (ord '0'))
               | (n>=10) && (n<=15) = chr (n - 10 + (ord 'a'))
               | otherwise = error "not a digit"    

 unwords' :: [String] -> String
 unwords' [] = ""
 unwords' (x:xs) = x ++ " " ++ (unwords' xs)

length' :: [a] -> Int
 length' [] = 0
 length' (h:t) = 1 + length' t

 div' :: Int -> Int -> Int
 div' n d | d > n = 0
          | otherwise = 1 + div' (n-d) d

 mod' :: Int -> Int -> Int
 mod' n d | d > n = n
          | otherwise = mod' (n-d) d

 divMod' :: Int -> Int -> (Int,Int)
 divMod' n d | d > n = (0,n)
             | otherwise = let (a,b) = divMod' (n-d) d
                           in (a+1,b)

 take' :: Int -> [a] -> [a]
 take' _ [] = []
 take' 0 _ = []
 take' n (x:xs) = x:take' (n-1) xs

 drop' :: Int -> [a] -> [a]
 drop' _ [] = []
 drop' 0 l = l
 drop' n (x:xs) = drop' (n-1) xs

 splitAt' :: Int -> [a] -> ([a],[a])
 splitAt' _ [] = ([],[])
 splitAt' 0 l = ([],l)
 splitAt' n (x:xs) = let (a,b) = splitAt' (n-1) xs
                     in (x:a,b)

 g :: Int -> Bool
 g n | n >= 10 = True
     | otherwise = False

 takeWhile' :: (a -> Bool) -> [a] -> [a]
 takeWhile' _ [] = []
 takeWhile' f (x:xs) | f x = x:takeWhile' f xs
                     | otherwise =  []

 dropWhile' :: (a -> Bool) -> [a] -> [a]
 dropWhile' _ [] = []
 dropWhile' f (x:xs) | f x = dropWhile' f xs
                     | otherwise = (x:xs)

 break' :: (a -> Bool) -> [a] -> ([a],[a])
 break' _ [] = ([],[])
 break' f (x:xs) | f x = let (a,b) = break' f xs
                         in (x:a,b)
                 | otherwise = ([],x:xs)

-- All functions explained 

-- isSpace' :: Char -> Bool: checks if a character is a space or not.
-- isUpper' :: Char -> Bool: checks if a character is an uppercase letter or not.
-- isAlpha' :: Char -> Bool: checks if a character is a letter (upper or lowercase).
-- toUpper' :: Char -> Char: converts a lowercase character to uppercase.
-- digitToInt' :: Char -> Int: converts a digit in character form to integer.
-- intToDigit' :: Int -> Char: converts an integer to digit character representation.
-- unwords' :: [String] -> String: concatenates a list of strings with a space in between.
-- length' :: [a] -> Int: returns the length of a list.
-- div' :: Int -> Int -> Int, mod' :: Int -> Int -> Int and divMod' :: Int -> Int -> (Int,Int) return the integer division and modulo results for 2 integers
-- takeWhile', dropWhile', break': return sublists of input list that retain/remove/split values wherever a condition specified by the input function is met.