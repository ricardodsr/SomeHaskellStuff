module BitToInt where

-- Define a type `Bit` as an alias for `Bool`
type Bit = Bool

-- Convert a `Bit` to an `Int`
-- Returns 0 if the input `Bit` is `False`
-- Returns 1 if the input `Bit` is `True`
bitToInt :: Bit -> Int
bitToInt False = 0
bitToInt True  = 1

-- Convert an `Int` to a `Bit`
-- Returns `False` if the input `Int` is 0
-- Returns `True` if the input `Int` is 1
intToBit :: Int -> Bit
intToBit 0 = False
intToBit 1 = True

-- Convert an `Int` to a list of `Bit`s
-- If the input `Int` is 0, returns an empty list
-- If the input `Int` is positive, performs integer division and modulo by 2 
-- and calls `intToBit` on the modulo result to get the next `Bit` in the list
intToBList :: Int -> [Bit]
intToBList 0 = []
intToBList n | n > 0 = ((intToBit b):(intToBList r))
    where (r,b) = divMod n 2

-- Convert a list of `Bit`s to an `Int`
-- If the input list is empty, returns 0
-- If the input list has elements, converts the head to an `Int` using `bitToInt`,
-- multiplies the result by 2 and adds it to the result of converting the tail of the list
bListToInt :: [Bit] -> Int
bListToInt [] = 0
bListToInt (h:t) = (bitToInt h) + 2*(bListToInt t)

-- An alternative implementation of `bListToInt` using `foldr`
bListToInt' l = foldr f 0 l
   where f h t = (bitToInt h) + 2*t

-- Perform a bitwise operation on two `Bit`s and return the result and a carry bit
-- Returns `(False, False)` if both inputs are `False`
-- Returns `(True, False)` for all other input combinations except for `(True, True)`
-- Returns `(True, True)` if both inputs are `True`
tabuada :: Bit -> Bit -> Bit -> (Bit, Bit) -- (res, carry)
tabuada False False False = (False, False)
tabuada False False True  = (True,  False)
tabuada False True  False = (True,  False)
tabuada True  False False = (True,  False)
tabuada False True  True  = (False, True )
tabuada True  False True  = (False, True )
tabuada True  True  False = (False, True )
tabuada True  True  True  = (True,  True )

-- soma is a function that takes two binary lists (as, bs) and returns the binary representation of their sum
soma :: [Bit] -> [Bit] -> [Bit]
soma as bs = fst (somaComCarry False as bs)

-- somaComCarry is a function that takes a carry bit, two binary lists (l1, l2) and returns their sum as a binary list and a carry bit
somaComCarry :: Bit -> [Bit] -> [Bit] -> ([Bit], Bit)
somaComCarry c (a:as) (b:bs) 
     = let (r,c') = tabuada a b c
           (x,y)  = somaComCarry c' as bs
       in (r:x,y)
-- If one of the binary lists has been fully processed, somaComCarry returns the sum after clearing the carry bit if it is set
somaComCarry c l1 l2 = (limpaCarry c (l1 ++ l2),False)

-- limpaCarry takes a carry bit and a binary list and returns the binary list after clearing the carry bit
limpaCarry True [] = [True]
limpaCarry True (True:t) = False:(limpaCarry True t)
limpaCarry True (False:t) = True:t
limpaCarry False t = t

-- multiplica takes two binary lists (n, m) and returns their binary representation of the product
multiplica :: [Bit]-> [Bit] -> [Bit]
multiplica n [b] = multiplicaB n b
multiplica n (h:t) = soma (multiplicaB n h) (False:(multiplica n t))
-- Or using the foldr function to calculate the product
multiplica' n1 n2 = foldr (f n1) [] n2
    where f n h t = soma (multiplicaB n h) (False:t)

-- multiplicaB takes a binary list and a binary digit and returns their binary representation of the product
multiplicaB x True = x
multiplicaB _ _ = []

-- mult takes two integers and returns their product
mult :: Int -> Int -> Int
mult x y = let bx = intToBList x
               by = intToBList y
               br = multiplica bx by
           in bListToInt br