module functions where

import Data.Char

-- Maximum of 2 numbers

--This function takes a tuple of two integers and returns the maximum value
 max2 :: (Int,Int) -> Int
 max2 (a,b) = if (a>b)
              then a
              else b

--This function takes a tuple of two integers and returns the maximum value using pattern matching
 max2a :: (Int,Int) -> Int
 max2a (a,b) | a > b = a
             | otherwise = b

--This function takes a tuple of two integers and returns the maximum value using pattern matching
 max2b :: (Int,Int) -> Int
 max2b c | fst c > snd c = fst c
         | otherwise = snd c

--This function takes a tuple of two integers and returns the maximum value using where clause
 max2c :: (Int,Int) -> Int
 max2c c = if a>b
           then a
           else b
       where a=fst c
                 b=snd c

--This function takes a tuple of two integers and returns the maximum value using let clause
 max2d :: (Int, Int) -> Int
 max2d c = let a = fst c
               b = snd c
               in if a> b
                  then a
                  else b

-- Maximum of 3 numbers
--This function takes a tuple of three integers and returns the maximum value
 max3 :: (Int,Int,Int) -> Int
 max3 (a,b,c) = max2 (max2 (a,b),c)

--This function takes a tuple of three integers and returns the maximum value
 max3b :: (Int,Int,Int) -> Int
 max3b (a,b,c) = if (a >= b) && (a >= c)
          then a
         else if b >= c
                      then b
                      else c

--This function takes a tuple of three integers and returns the maximum value using pattern matching
 max3c :: (Int,Int,Int) -> Int
 max3c (a,b,c) | (a>=b) && (a>=c) = a
               | (b>=a) && (b>=c) = b
           | otherwise = c


-- tri function checks if three sides can form a triangle
tri :: (Float,Float,Float) -> Bool
tri (a,b,c) = if (a+b > c) && (a+c > b) && (b+c > a)
               then True  -- return True if the sides can form a triangle
               else False -- return False if the sides can't form a triangle

-- opp function takes a pair (Int, (Int,Int)) and returns the result of an operation on its second component
opp :: (Int, (Int,Int)) -> Int
opp z = if ((fst z) == 1)
         then (fst (snd z)) + (snd (snd z))  -- return the sum if the first component of the pair is 1
         else if ((fst z) == 2)
              then (fst (snd z)) - (snd (snd z))  -- return the difference if the first component of the pair is 2
              else 0  -- return 0 if the first component of the pair is anything other than 1 or 2

-- oppa is a simplified version of opp
oppa :: (Int , (Int,Int)) -> Int 
oppa (1,(b1,b2)) = b1 + b2 -- return the sum if the first component of the pair is 1
oppa (2,(b1,b2)) = b1 - b2 -- return the difference if the first component of the pair is 2
oppa (a,b) = 0  -- return 0 if the first component of the pair is anything other than 1 or 2

-- numr returns the number of roots of a quadratic equation
numr :: Float -> Float -> Float -> Float
numr a b c = let x = b^2 - 4*a*c
              in if x < 0
                 then 0  -- return 0 if the equation has no real roots
                 else if x == 0
                      then 1  -- return 1 if the equation has one real root
                      else 2  -- return 2 if the equation has two real roots

-- raizes returns a list of the roots of a quadratic equation
raizes :: Float -> Float -> Float -> [Float]
raizes a b c | numr a b c == 0 = []  -- return an empty list if the equation has no real roots
              | numr a b c == 1 = [ -b / (2 * a) ]  -- return a list with one root if the equation has one real root
              | otherwise = [ (-b - r) / (2*a), (-b +r) / (2*a) ]  -- return a list with two roots if the equation has two real roots
                            where r = sqrt (b^2 - 4*a*c)

-- isSpace' checks if a character is a space
isSpace' :: Char -> Bool
isSpace' c = if (c==' ') then True else False

-- isUpper' checks if a character is an uppercase letter
isUpper' :: Char -> Bool
isUpper' c = (c >= 'A') && (c <= 'Z')

-- isAlpha' checks if a character is a letter
isAlpha' :: Char -> Bool
isAlpha' c = (isUpper c) || (isLower c)

-- toUpper' function takes a character c as input and returns an uppercase version of the character if it is a lowercase letter, else returns the character itself.
toUpper' :: Char -> Char
toUpper' c = if (isLower c) -- if c is a lowercase letter
then chr ((ord 'A')+(ord c)-(ord 'a')) -- convert to uppercase by subtracting the ASCII value of a and adding the ASCII value of A
else c -- return c as it is if it is already an uppercase letter or not an alphabet

-- digitToInt' function takes a character c as input and returns the corresponding integer representation of the digit if c is a digit or a hexadecimal character, else raises an error "not a digit."
digitToInt' :: Char -> Int
digitToInt' c | isDigit c = (ord c) - (ord '0') -- if c is a decimal digit, return the integer representation by subtracting the ASCII value of 0
| (c >= 'a') && (c<='f') = (ord c) - (ord 'a') + 10 -- if c is a lowercase hexadecimal digit, return the integer representation by subtracting the ASCII value of a and adding 10
| (c >= 'A') && (c<='F') = (ord c) - (ord 'A') + 10 -- if c is an uppercase hexadecimal digit, return the integer representation by subtracting the ASCII value of A and adding 10
| otherwise = error "not a digit." -- raise an error if c is not a digit or a hexadecimal character

-- intToDigit' function takes an integer n as input and returns the corresponding character representation of the digit if n is in the range [0, 9] or [10, 15], else raises an error "not a digit".
intToDigit' :: Int -> Char
intToDigit' n | (n>=0) && (n<=9) = chr (n + (ord '0')) -- if n is in the range [0, 9], return the character representation by adding the ASCII value of 0
| (n>=10) && (n<=15) = chr (n - 10 + (ord 'a')) -- if n is in the range [10, 15], return the character representation by subtracting 10 and adding the ASCII value of a
| otherwise = error "not a digit" -- raise an error if n is not in the specified range

-- unwords' function takes a list of strings [String] as input and returns a concatenated string formed by inserting a space character between each pair of words.
unwords' :: [String] -> String
unwords' [] = "" -- return an empty string if the input list is empty
unwords' (x:xs) = x ++ " " ++ (unwords' xs) -- concatenate the first word x with a space and the result of recursively calling unwords' on the rest of the list xs

-- 'limpa' is a function that takes a list of strings as input and returns a list of strings.
-- The function removes all empty strings from the input list.
limpa :: [String] -> [String]
limpa [] = []
limpa (x:xs) = if (x == "")
then limpa xs
else x:(limpa xs)

-- 'paragrafo' is a function that takes a string as input and returns a list of strings.
-- The function splits the input string into separate lines (using the 'lines' function)
-- and removes all empty strings from the list of lines using the 'limpa' function.
paragrafo :: String -> [String]
paragrafo s = let linhas = lines s
in limpa linhas

-- 'separa' is a function that takes a string as input and returns a tuple of two strings.
-- The function splits the input string into two parts, using the first dot ('.') as the separator.
-- The first part of the tuple is everything before the dot, and the second part is everything after the dot.
-- If there is no dot in the input string, the function returns an empty string as the first part of the tuple.
separa :: String -> (String,String)
separa "" = ("","")
separa ('.':xs) = ("",xs)
separa (x:xs) = let (a,b) = separa xs
in (x:a,b)

-- 'frases' is a function that takes a string as input and returns a list of strings.
-- The function splits the input string into separate sentences using the 'separa' function.
frases :: String -> [String]
frases "" = []
frases s = let (a,b) = separa s
in a:(frases b)


-- | length' calculates the length of a list.
length' :: [a] -> Int
-- | Base case, the length of an empty list is 0.
length' [] = 0
-- | Recursive case, the length of a non-empty list is 1 plus the length of its tail.
length' (h:t) = 1 + length' t

-- | div' calculates the integer division of two integers.
div' :: Int -> Int -> Int
-- | If the divisor is greater than the dividend, the result is 0.
div' n d | d > n = 0
-- | Otherwise, the result is 1 plus the result of dividing the difference of the dividend and the divisor.
div' n d | otherwise = 1 + div' (n-d) d

-- | mod' calculates the modulo of two integers.
mod' :: Int -> Int -> Int
-- | If the divisor is greater than the dividend, the result is the dividend.
mod' n d | d > n = n
-- | Otherwise, the result is the modulo of the difference of the dividend and the divisor.
mod' n d | otherwise = mod' (n-d) d

-- | divMod' calculates the tuple of the integer division and modulo of two integers.
divMod' :: Int -> Int -> (Int,Int)
-- | If the divisor is greater than the dividend, the result is a tuple of 0 and the dividend.
divMod' n d | d > n = (0,n)
-- | Otherwise, the result is the result of dividing and taking the modulo of the difference of the dividend and the divisor.
divMod' n d | otherwise = let (a,b) = divMod' (n-d) d
                           in (a+1,b)

-- | take' returns the first n elements of a list.
take' :: Int -> [a] -> [a]
-- | Base case, returns an empty list if n is 0 or the list is empty.
take' _ [] = []
take' 0 _ = []
-- | Recursive case, returns the first element of the list followed by the result of taking n-1 elements from the tail of the list.
take' n (x:xs) = x:take' (n-1) xs

-- | drop' removes the first n elements of a list.
drop' :: Int -> [a] -> [a]
-- | Base case, returns an empty list if the list is empty.
drop' _ [] = []
-- | If n is 0, returns the original list.
drop' 0 l = l
-- | Recursive case, returns the result of dropping n-1 elements from the tail of the list.
drop' n (x:xs) = drop' (n-1) xs

-- splitAt' takes an integer 'n' and a list of elements 'l', and splits the list 'l' into two parts at the nth position
splitAt' :: Int -> [a] -> ([a],[a])

-- If the list 'l' is empty, return two empty lists
splitAt' _ [] = ([],[])

-- If the position 'n' is 0, return an empty list as the first part and the original list as the second part
splitAt' 0 l = ([],l)

-- If the position 'n' is not 0, split the list at the (n-1)th position and add the first element to the first part and the rest to the second part
splitAt' n (x:xs) = let (a,b) = splitAt' (n-1) xs
                     in (x:a,b)
                     
-- g takes an integer 'n' and returns True if 'n' is greater than or equal to 10, and False otherwise
g :: Int -> Bool
g n | n >= 10 = True
    | otherwise = False

-- takeWhile' takes a function 'f' and a list 'l', and returns a list of elements from the start of 'l' as long as 'f x' is True for each element 'x'
takeWhile' :: (a -> Bool) -> [a] -> [a]

-- If the list 'l' is empty, return an empty list
takeWhile' _ [] = []

-- If 'f x' is True, add the element 'x' to the result list and repeat the process for the rest of the list
takeWhile' f (x:xs) | f x = x:takeWhile' f xs

-- If 'f x' is False, return an empty list
                    | otherwise =  []

-- dropWhile' takes a function 'f' and a list 'l', and returns a list of elements from the start of 'l' as long as 'f x' is False for each element 'x'
dropWhile' :: (a -> Bool) -> [a] -> [a]

-- If the list 'l' is empty, return an empty list
dropWhile' _ [] = []

-- If 'f x' is True, drop the element 'x' and repeat the process for the rest of the list
dropWhile' f (x:xs) | f x = dropWhile' f xs

-- If 'f x' is False, return the rest of the list
                    | otherwise = (x:xs)

-- break' takes a function 'f' and a list 'l', and splits the list 'l' into two parts where the first part consists of elements for which 'f x' is True and the second part consists of elements for which 'f x' is False
break' :: (a -> Bool) -> [a] -> ([a],[a])

-- If the list 'l' is empty, return two empty lists
break' _ [] = ([],[])

-- If 'f x' is True, add the element 'x' to the first part and repeat the process for the rest of the list
break' f (x:xs) | f x = let (a,b) = break' f xs
                         in (x:a,b)
                | otherwise = ([],x:xs)
				 
			


 type Coeficiente = Int
 type Polinomio = [Coeficiente]

 somapol :: Polinomio -> Polinomio -> Polinomio
 somapol [] l = l
 somapol l [] = l
 somapol (x:xs) (y:ys) = (x+y):somapol xs ys

 mulpol :: Polinomio -> Polinomio -> Polinomio
 mulpol p1 p2 = somalistas (geralistas p1 p2 [])

 somalistas :: [Polinomio] -> Polinomio
 somalistas l = foldr somapol [] l

 geralistas :: Polinomio -> Polinomio -> [Coeficiente] -> [Polinomio]
 geralistas p [] _ = []
 geralistas [] p _ = []
 geralistas p (x:xs) cs = (cs++(map (*x) p)):geralistas p xs (0:cs)

  data ExpInt = Const Int 
	     | Simetrico ExpInt 
             | Mais ExpInt ExpInt 
             | Menos ExpInt ExpInt 
             | Mult ExpInt ExpInt

 ex1 :: ExpInt
 ex1 = (Mais (Mult (Mais (Const 3) (Const 5)) (Menos (Const 4) (Const 2))) (Simetrico (Const 1)))

 calcula :: ExpInt -> Int
 calcula (Const x) = x
 calcula (Simetrico x) = negate (calcula x)
 calcula (Mais x y) = (calcula x) + (calcula y)
 calcula (Menos x y) = (calcula x) - (calcula y)
 calcula (Mult x y) = (calcula x) * (calcula y)

 expString :: ExpInt -> String
 expString (Const x) = show x
 expString (Simetrico x) = "-(" ++ expString x ++ ")"
 expString (Mais x y) = "(" ++ (expString x) ++ "+" ++ (expString y)  ++ ")"
 expString (Menos x y) = "(" ++ (expString x) ++ "-" ++ (expString y)  ++ ")"
 expString (Mult x y) = "(" ++ (expString x) ++ "*" ++ (expString y)  ++ ")"

 posfix :: ExpInt -> String
 posfix (Const x) = show x ++ " "
 posfix (Simetrico x) = posfix x ++ " -/+ "
 posfix (Mais x y) =  (posfix x) ++ (posfix y)  ++ " + "
 posfix (Menos x y) = (posfix x) ++ (posfix y)  ++ " - "
 posfix (Mult x y) =  (posfix x) ++ (posfix y)  ++ " * "

 type ExpN = [Parcela] 
 type Parcela = [Int] 

 calcN :: ExpN -> Int
 calcN l = sum (map product l)

 normaliza :: ExpInt -> ExpN
 normaliza (Const x) = [[x]]
 normaliza (Simetrico x) = map ((-1):) (normaliza x)
 normaliza (Mais x y) = (normaliza x) ++ (normaliza y)
 normaliza (Menos x y) = normaliza (Mais x (Simetrico y))
 normaliza (Mult x y) = let xN = normaliza x
                            yN = normaliza y
                        in [ l1 ++ l2 | l1 <- xN, l2 <- yN]
		        --in distrib xN yN

 distrib :: ExpN -> ExpN -> ExpN
 distrib [] _ = []
 distrib (x:xs) l = (map (x++) l) ++ (distrib xs l)
 
 expNString :: ExpN -> String
 expNString [] = ""
 expNString [x] = prodNString x
 expNString (x:xs) = (prodNString x) ++ "+" ++ expNString xs

 prodNString :: Parcela -> String
 prodNString [] = ""
 prodNString [x] = show x
 prodNString (x:xs) = (show x) ++ "*" ++ prodNString xs 

 simplifica :: ExpInt -> String 
 simplifica e = expNString (normaliza e)
data Exp a = Const a
           | Simetrico (Exp a)
           | Mais  (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Vezes (Exp a) (Exp a)
type ExpInt = Exp Int

calcula :: (Num a) => Exp a -> a
calcula (Const x) = x
calcula (Simetrico x) = negate (calcula x)
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Vezes x y) = (calcula x) * (calcula y)

expStr :: (Num a) => Exp a -> String
expStr (Const x) = show x
expStr (Simetrico x) = "-(" ++ (expStr x) ++ ")"
expStr (Mais x y)    = "("  ++ (expStr x) ++ " + " ++ (expStr y) ++ ")"
expStr (Menos x y)   = "("  ++ (expStr x) ++ " - " ++ (expStr y) ++ ")"
expStr (Vezes x y)   = "("  ++ (expStr x) ++ " * " ++ (expStr y) ++ ")"


posfix :: (Num a) => Exp a -> String 
posfix (Const x) = show x
posfix (Simetrico x) = (posfix x) ++ " -/+ "
posfix (Mais x y)    = (posfix x) ++ (posfix y) ++ " + "
posfix (Menos x y)   = (posfix x) ++ (posfix y) ++ " - "
posfix (Vezes x y)   = (posfix x) ++ (posfix y) ++ " * "


type ExpN a = [Parcela a]
type Parcela a = [a]

calcN :: (Num a) => ExpN a -> a
calcN = sum . (map product )

normaliza :: (Num a) => Exp a -> ExpN a 
normaliza (Const x) = [[x]]
normaliza (Mais x y) = (normaliza x) ++ (normaliza y)
normaliza (Menos x y) = normaliza (Mais x (Simetrico y))
normaliza (Vezes x y) = [a ++ b | a <- normaliza x
                                , b <- normaliza y]
normaliza (Simetrico x) = map invertePrimeiro (normaliza x)
     where invertePrimeiro [] = []
           invertePrimeiro (h:t) = (negate h):t

expNString :: (Show a) => ExpN a -> String
expNString = (junta " + ") . map ((junta " * ") . (map show)) 
   where junta _ [] = []
         junta _ [x] = x
         junta s (h:t) = h ++ s ++ (junta s t)

-- A função junta pode tb ser definida de uma outra forma mais compacta 
junta1 :: String -> [String] -> String
junta1 s l = concat (zipWith (++) ("":repeat s) l)

junta2 :: String -> [String] -> String
junta2 = (concat .) . (zipWith (++)) . (("":).repeat)

simplifica :: (Show a, Num a) => Exp a -> String
simplifica = expNString . normaliza

instance Show a => Show (Exp a) where
  showsPrec _ (Const x) = showsPrec 0 x
  showsPrec _ (Simetrico e) = showString "-(" . showsPrec 0 e . showString ")"
  showsPrec _ (Mais e1 e2) = showString "(" . showsPrec 0 e1 . showString " + " . 
                       showsPrec 0 e2 . showString ")"
  showsPrec _ (Menos e1 e2) = showString "(" . showsPrec 0 e1 . showString " - " . 
                       showsPrec 0 e2 . showString ")"
  showsPrec _ (Vezes e1 e2) = showString "(" . showsPrec 0 e1 . showString " * " . 
                       showsPrec 0 e2 . showString ")"
instance (Eq a,Num a) => Eq (Exp a) where
   e1 == e2 = (calcula e1) == (calcula e2)

instance (Num a) => Num (Exp a) where
   (+) = Mais
   (-) = Menos
   (*) = Vezes
   negate = Simetrico
   signum e1 = Const (signum (calcula e1))
   abs x = Vezes (signum x) (x)
   fromInteger x = Const (fromInteger x)

unposfix :: (Read a, Num a) => String -> Exp a
unposfix = unposfixAux []

unposfixAux l "" = head l
unposfixAux l s = let ((x,y):_) = lex s
                      (a:b:l') = l
                      (c:l'') = l 
                  in case x of
                     "*"   -> unposfixAux ((Vezes b a):l') y
                     "+"   -> unposfixAux ((Mais b a):l') y
                     "-"   -> unposfixAux ((Menos b a):l') y
                     "+/-" -> unposfixAux ((Simetrico c):l'') y
                     _     -> unposfixAux ((Const (read x)):l) y
calculaP = calcula . unposfix

unposFix :: (Read a, Num a) => String -> Exp a
unposFix = unposFixAux [] []

unposFixAux m l "" = head l
unposFixAux m l s = let ((x,y):_) = lex s
                        (a:b:l') = l
                        (c:l'') = l 
                        Just v = lookup x m
                    in case x of
                       "*"   -> unposFixAux m ((Vezes b a):l') y
                       "+"   -> unposFixAux m ((Mais b a):l') y
                       "-"   -> unposFixAux m ((Menos b a):l') y
                       "+/-" -> unposFixAux m ((Simetrico c):l'') y
                       "@"   -> let ((z,w):_) = lex y
                                in unposFixAux ((z,c):m) l w
                       _     -> if (isDigit (head x))
                                then unposFixAux m ((Const (read x)):l) y
                                else unposFixAux m (v:l) y
calculaPP = calcula . unposFix

calculaPPP :: (Read a, Num a) => String -> a
calculaPPP = calcAux [] [] 

calcAux m l "" = head l
calcAux m l s = let ((x,y):_) = lex s
                    (a:b:l') = l
                    (c:l'') = l 
                    Just v = lookup x m
                in case x of
                   "*"   -> calcAux m ((b * a):l') y
                   "+"   -> calcAux m ((b + a):l') y
                   "-"   -> calcAux m ((b - a):l') y
                   "+/-" -> calcAux m ((negate c):l'') y
                   "@"   -> let ((z,w):_) = lex y
                            in calcAux ((z,c):m) l w
                   _     -> if (isDigit (head x))
                            then calcAux m ((read x):l) y
                            else calcAux m (v:l) y

 class FF f where 
	dom :: (Ord a) => (f a b) -> [a] 
	procura :: (Ord a) => (f a b) -> a -> Maybe b 
	acrescenta :: (Ord a) => (f a b) -> a -> b -> f a b 
	remove :: (Ord a) => (f a b) -> a -> f a b 
	lista :: (Ord a) => (f a b) -> [(a,b)] 

 data (Ord a) => LOrd a b = L [(a,b)] 
 
 data (Ord a) => ABP a b = V | N (a,b) (ABP a b) (ABP a b) 

 instance FF LOrd where 
	dom (L l) = map fst l 
	procura (L l) x = case (takeWhile ((==x).fst) (dropWhile ((<x).fst) l)) of 
		((_,b):_) -> Just b 
		[] -> Nothing
	acrescenta (L l) x y = let (a,b) = break ((<x) .fst) l 
				in L (a ++ ((x,y):(dropWhile ((==x).fst) b))) 
	remove (L l) x = let (a,b) = break ((<x) .fst) l 
				in L (a ++ (dropWhile ((==x).fst) b)) 
	lista (L l) = l 


 instance FF ABP where
	dom V = []
        dom (N (x,y) e d) = (dom e) ++ (x : (dom d))
	procura V _ = Nothing
        procura (N (x,y) e d) x' | x' == x = Just y
			         | x' < x = procura e x'
                                 | otherwise = procura d x'
	acrescenta V x y = N (x,y) V V
        acrescenta (N (x,y) e d) x' y' | x' == x = N (x',y') e d
				       | x' < x = N (x,y) (acrescenta e x' y') d
                                       | otherwise = N (x,y) e (acrescenta d x' y')
        lista V = []
        lista (N par e d) = (lista e) ++ (par:(lista d))
	remove V x = V
        remove (N (x,y) V d) x' | x' == x = d
                                | x' < x = N (x,y) V d
                                | x' > x = N (x,y) V (remove d x')
	remove (N (x,y) e V) x' | x' == x = e
                                | x' > x = N (x,y) e V
                                | x' < x = N (x,y) (remove e x') V
	remove (N (x,y) e d) x' | x' == x = let (a,b) = menor d
                                                menor (N (x,y) V _) = (x,y)
                                                menor (N _ e _) = menor e
                                          in N (a,b) e (remove d a)
                                | x' > x = N (x,y) e (remove d x')
                                | x' < x = N (x,y) (remove e x') d



