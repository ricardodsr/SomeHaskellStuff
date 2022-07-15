module basic where
    import Data.Char

    
--Maximum of 2 number
-- if then else Basic 
-- Tomada dde decisao com if then else
max2 :: (Int,Int) -> Int
max2 (a,b) = if (a>b)
              then a
              else b

--Maximum of 2 number
--Guardas
max2a :: (Int,Int) -> Int
max2a (a,b) | a > b = a
            | otherwise = b

--Maximum of 2 number
--using fst and snd
max2b :: (Int,Int) -> Int
max2b c  | fst c > snd c = fst c
         | otherwise = snd c


--Maximum of 2 number
--where 
max2c :: (Int,Int) -> Int
max2c c = if a>b
           then a
           else b
	   where a=fst c
                 b=snd c

--Maximum of 2 number
--using let in
max2d :: (Int, Int) -> Int
max2d c = let a = fst c
               b = snd c
               in if a> b
                  then a
                  else b

-- Maximum of 3 numbers 
--using max2
 max3 :: (Int,Int,Int) -> Int
 max3 (a,b,c) = max2 (max2 (a,b),c)

 max3b :: (Int,Int,Int) -> Int
 max3b (a,b,c) = if (a >= b) && (a >= c)
 		 then a
		 else if b >= c
                      then b
                      else c

-- Maximum of 3 numbers 
--guardas
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

--Formula resolvente
--using ig then else
 numr :: Float -> Float -> Float -> Float
 numr a b c = let x = b^2 - 4*a*c
              in if x < 0
                 then 0
                 else if x == 0
                      then 1
                      else 2


--Formula resolvente
--guardas
 raizes :: Float -> Float -> Float -> [Float]
 raizes a b c | numr a b c == 0 = []
              | numr a b c == 1 = [ -b / (2 * a) ]
              | otherwise = [ (-b - r) / (2*a), (-b +r) / (2*a) ]
                            where r = sqrt (b^2 - 4*a*c)

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