module functions where

import Data.Char

 max2 :: (Int,Int) -> Int
 max2 (a,b) = if (a>b)
              then a
              else b

 max2a :: (Int,Int) -> Int
 max2a (a,b) | a > b = a
             | otherwise = b


 max2b :: (Int,Int) -> Int
 max2b c | fst c > snd c = fst c
         | otherwise = snd c



 max2c :: (Int,Int) -> Int
 max2c c = if a>b
           then a
           else b
	   where a=fst c
                 b=snd c

 max2d :: (Int, Int) -> Int
 max2d c = let a = fst c
               b = snd c
               in if a> b
                  then a
                  else b

 max3 :: (Int,Int,Int) -> Int
 max3 (a,b,c) = max2 (max2 (a,b),c)

 max3b :: (Int,Int,Int) -> Int
 max3b (a,b,c) = if (a >= b) && (a >= c)
 		 then a
		 else if b >= c
                      then b
                      else c

 max3c :: (Int,Int,Int) -> Int
 max3c (a,b,c) | (a>=b) && (a>=c) = a
               | (b>=a) && (b>=c) = b
	       | otherwise = c

 tri :: (Float,Float,Float) -> Bool
 tri (a,b,c) = if (a+b > c) && (a+c > b) && (b+c > a)
               then True
               else False

 opp :: (Int, (Int,Int)) -> Int
 opp z = if ((fst z) == 1)
         then (fst (snd z)) + (snd (snd z))
         else if ((fst z) == 2)
              then (fst (snd z)) - (snd (snd z))
              else 0

 oppa :: (Int , (Int,Int)) -> Int 
 oppa (1,(b1,b2)) = b1 + b2
 oppa (2,(b1,b2)) = b1 - b2
 oppa (a,b) = 0

 numr :: Float -> Float -> Float -> Float
 numr a b c = let x = b^2 - 4*a*c
              in if x < 0
                 then 0
                 else if x == 0
                      then 1
                      else 2

 raizes :: Float -> Float -> Float -> [Float]
 raizes a b c | numr a b c == 0 = []
              | numr a b c == 1 = [ -b / (2 * a) ]
              | otherwise = [ (-b - r) / (2*a), (-b +r) / (2*a) ]
                            where r = sqrt (b^2 - 4*a*c)
 
 
 --------------------------------------------- Viagens
 type Horas = (Int,Int)
 type Etapa = (Horas,Horas)
 type Viagem = [Etapa]

 e1 = ((9,30), (10,25)) :: Etapa
 e2 = ((11,30), (10,25)) :: Etapa
 v1 = [((9,30), (10,25)), ((11,20), (12,45)) , ((13,30), (14,45))] :: Viagem
 v2 = [((9,30), (10,25)), ((11,20), (14,00)) , ((13,30), (14,45))] :: Viagem
 v3 = [((9,30), (10,25)), ((11,20), (12,45)) , ((13,30), (11,45))] :: Viagem

 maisTarde :: Horas -> Horas -> Bool
 maisTarde (h1,m1) (h2,m2) | (h1 > h2) = True
			   | (h1 < h2) = False
                           | (m1 > m2) = True
                           | otherwise = False

 diferenca :: Horas -> Horas -> Int
 diferenca (h1,m1) (h2,m2) = let min1 = h1 * 60 + m1
                                 min2 = h2 * 60 + m2
                             in min1-min2


 testaEtapa :: Etapa -> Bool
 testaEtapa (p,c) = maisTarde c p

 testaLigacao :: Etapa -> Etapa -> Bool
 testaLigacao et1@(p1,c1) et2@(p2,c2) = (testaEtapa et1) && (testaEtapa et2) && (maisTarde p2 c1)

 testaViagem :: Viagem -> Bool
 testaViagem [] = True
 testaViagem (et1:[]) = testaEtapa et1
 testaViagem (eta:etb:xs) = (testaLigacao eta etb) && (testaViagem (etb:xs))

 tempoEtapa :: Etapa -> Int
 tempoEtapa (p,c) = diferenca c p

 tempoEfectivo :: Viagem -> Int
 tempoEfectivo [] = 0
 tempoEfectivo (e:es) = (tempoEtapa e) + tempoEfectivo es

-- tempoEspera :: Viagem -> Int

 tempoTotal :: Viagem -> Int
-- tempoTotal v = tempoEspera v + tempoEfectivo v
 tempoTotal v = let (p1,c1) = head v
                    (pn,cn) = last v
                in diferenca cn p1




-- testaViagem (et2:et1:[]) = testaLigacao et2 et1
-- testaViagem (et3:et2:et1:[]) = (testaLigacao et3 et2) && (testaLigacao et2 et1)
-- testaViagem (et4:et3:et2:et1:[]) = (testaLigacao et4 et3) && (testaLigacao et3 et2) && (testaLigacao et2 et1)

----------------------------------------

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

 limpa :: [String] -> [String]
 limpa [] = []
 limpa (x:xs) = if (x == "")
                then limpa xs
                else x:(limpa xs)

 paragrafo :: String -> [String]
 paragrafo s = let linhas = lines s
--                in [ x | x <- linhas , x /= "" ]
               in limpa linhas 

 separa :: String -> (String,String)
 separa "" = ("","")
 separa ('.':xs) = ("",xs)
 separa (x:xs) = let (a,b) = separa xs
                 in (x:a,b)

 frases :: String -> [String]
 frases "" = []
 frases s = let (a,b) = separa s
            in a:(frases b)


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
				 
			

type Bit = Bool
bitToInt :: Bit -> Int
bitToInt False = 0
bitToInt True  = 1

intToBit :: Int -> Bit
intToBit 0 = False
intToBit 1 = True

intToBList :: Int -> [Bit]
intToBList 0 = []
intToBList n | n > 0 = ((intToBit b):(intToBList r))
    where (r,b) = divMod n 2

bListToInt :: [Bit] -> Int
bListToInt [] = 0
bListToInt (h:t) = (bitToInt h) + 2*(bListToInt t)
--ou entao
bListToInt' l = foldr f 0 l
   where f h t = (bitToInt h) + 2*t

tabuada :: Bit -> Bit -> Bit -> (Bit, Bit) -- (res, carry)
tabuada False False False = (False, False)
tabuada False False True  = (True,  False)
tabuada False True  False = (True,  False)
tabuada True  False False = (True,  False)
tabuada False True  True  = (False, True )
tabuada True  False True  = (False, True )
tabuada True  True  False = (False, True )
tabuada True  True  True  = (True,  True )

soma :: [Bit] -> [Bit] -> [Bit]
soma as bs = fst (somaComCarry False as bs)

somaComCarry :: Bit -> [Bit] -> [Bit] -> ([Bit], Bit)
somaComCarry c (a:as) (b:bs) 
     = let (r,c') = tabuada a b c
           (x,y)  = somaComCarry c' as bs
       in (r:x,y)
somaComCarry c l1 l2 = (limpaCarry c (l1 ++ l2),False)

limpaCarry True [] = [True]
limpaCarry True (True:t) = False:(limpaCarry True t)
limpaCarry True (False:t) = True:t
limpaCarry False t = t

multiplica :: [Bit]-> [Bit] -> [Bit]
multiplica n [b] = multiplicaB n b
multiplica n (h:t) = soma (multiplicaB n h) (False:(multiplica n t))
--ou entao
multiplica' n1 n2 = foldr (f n1) [] n2
    where f n h t = soma (multiplicaB n h) (False:t)
multiplicaB x True = x
multiplicaB _ _ = []

mult :: Int -> Int -> Int
mult x y = let bx = intToBList x
               by = intToBList y
               br = multiplica bx by
           in bListToInt br

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



