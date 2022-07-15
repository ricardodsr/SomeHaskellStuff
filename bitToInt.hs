module bittoint where
    
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