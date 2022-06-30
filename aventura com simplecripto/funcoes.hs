module Funcoes where
import Tipos
import Char
import System.Random -- Isto e para a funcao dado
import Base


instance Show Agente where
 show (A f d i c e) =      "\n\tForca: "++(show f)++"\n"++
                     	   "\tDestreza: "++(show d)++"\n"++
                           "\tInteligencia: "++(show i)++"\n"++
                           "\tCarisma: "++(show c)++"\n\n"++
                           "\tFundos: "++(show e)++"\n"

playerskill :: IO Agente
playerskill = do{
                     f<-atributos;
                     d<-atributos;
                     i<-atributos;
                     c<-atributos;
                     fund<-fundos;
                     return((A f d i c fund))
                    }              

atributos :: IO Int
atributos = do {
		x<-rodaDados 4 6;
		return(sum(tail(qsort(x))))
               }

fundos :: IO Int
fundos = do {
               x<-dado 100;
               return(x+399)
              }

qsort :: [Int] -> [Int]
qsort [] = []
qsort (h:t) = let t1 = filter (<=h) t
                  t2 = filter (>h) t
              in (qsort t1) ++ [h] ++ (qsort t2)


rodaDados :: Int -> Int -> IO [Int]
rodaDados 0 _= return []
rodaDados n k = do { 
                     x <- dado k;
                     y <- rodaDados (n-1) k;
                     return (x:y)
                   }

dado :: Int -> IO Int
dado x = getStdRandom (randomR (1,x))

verificarFundos :: Int -> Int -> IO Bool
verificarFundos afund x = do return(afund>=x)

abilidades :: Int -> Int -> IO Bool
abilidades x meta = do{
                     roll<-dado 20;                                   
                     return(((div (x-10) 2) + roll)>=meta)
                    }

playerAg :: Agente -> IO()
playerAg a = do putStr (show a)


makepal :: Palavra -> String
makepal [] = ""
makepal (x:xs) | (x=='U') && ((vepal 0 xs)=='M') = "1 " ++ (makepal xs)
	       | (x=='D') && ((vepal 0 xs)=='E') && ((vepal 1 xs)=='Z') && ((vepal 2 xs)=='A') = "1" ++ (makepal xs)
   	       | (x=='D') && ((vepal 0 xs)=='E') && ((vepal 1 xs)=='Z') && ((vepal 2 xs)=='O') = "1" ++ (makepal xs) 
	       
               | (x=='D') && ((vepal 0 xs)=='O') && ((vepal 1 xs)=='I') && ((vepal 2 xs)=='S') = "2 " ++ (makepal xs)
               | (x=='T') && ((vepal 0 xs)=='R') && ((vepal 1 xs)=='E') && ((vepal 2 xs)=='S') = "3 " ++ (makepal xs)
               | (x=='Q') && ((vepal 0 xs)=='U') && ((vepal 1 xs)=='A') && ((vepal 2 xs)=='T') && ((vepal 3 xs)=='R') && ((vepal 4 xs)=='O') = "4 " ++ (makepal xs)
               | (x=='C') && ((vepal 0 xs)=='I') && ((vepal 1 xs)=='N') && ((vepal 2 xs)=='C') && ((vepal 3 xs)=='O')= "5 " ++ (makepal xs)
               | (x=='S') && ((vepal 0 xs)=='E') && ((vepal 1 xs)=='I') && ((vepal 2 xs)=='S') = "6 " ++ (makepal xs)
               | (x=='S') && ((vepal 0 xs)=='E') && ((vepal 1 xs)=='T') && ((vepal 2 xs)=='E') = "7 " ++ (makepal xs)
               | (x=='O') && ((vepal 0 xs)=='I') && ((vepal 1 xs)=='T') && ((vepal 2 xs)=='O') = "8 " ++ (makepal xs)
               | (x=='N') && ((vepal 0 xs)=='O') && ((vepal 1 xs)=='V') && ((vepal 2 xs)=='E') = "9 " ++ (makepal xs)
               | (x=='Z') && ((vepal 0 xs)=='E') && ((vepal 1 xs)=='R') && ((vepal 2 xs)=='O') = "0 " ++ (makepal xs)
               | (x=='D') && ((vepal 0 xs)=='E') && ((vepal 1 xs)=='Z') = "10 " ++ (makepal xs)
	       | (x=='O') && ((vepal 0 xs)=='N') && ((vepal 1 xs)=='Z') && ((vepal 2 xs)=='E') = "11 " ++ (makepal xs)
	       | (x=='D') && ((vepal 0 xs)=='O') && ((vepal 1 xs)=='Z') && ((vepal 2 xs)=='E') = "12 " ++ (makepal xs)
	       | (x=='T') && ((vepal 0 xs)=='R') && ((vepal 1 xs)=='E') && ((vepal 2 xs)=='Z') && ((vepal 3 xs)=='E') = "13 " ++ (makepal xs)
	       | (x=='Q') && ((vepal 0 xs)=='U') && ((vepal 1 xs)=='A') && ((vepal 2 xs)=='T') && ((vepal 3 xs)=='O') && ((vepal 4 xs)=='R') && ((vepal 5 xs)=='Z') && ((vepal 6 xs)=='E') = "14 " ++ (makepal xs)
               | (x=='C') && ((vepal 0 xs)=='A') && ((vepal 1 xs)=='T') && ((vepal 2 xs)=='O') && ((vepal 3 xs)=='R') && ((vepal 4 xs)=='Z') && ((vepal 5 xs)=='E') = "14 " ++ (makepal xs)
	       | (x=='Q') && ((vepal 0 xs)=='U') && ((vepal 1 xs)=='I') && ((vepal 2 xs)=='N') && ((vepal 3 xs)=='Z') && ((vepal 4 xs)=='E') = "15 " ++ (makepal xs)
	       | (x=='V') && ((vepal 0 xs)=='I') && ((vepal 1 xs)=='N') && ((vepal 2 xs)=='T') && ((vepal 3 xs)=='E') && ((vepal 4 xs)==' ') && ((vepal 5 xs)=='E') = "2" ++ (makepal xs)
               | (x=='V') && ((vepal 0 xs)=='I') && ((vepal 1 xs)=='N') && ((vepal 2 xs)=='T') && ((vepal 3 xs)=='E') = "20 " ++ (makepal xs)
	       | (x=='T') && ((vepal 0 xs)=='R') && ((vepal 1 xs)=='I') && ((vepal 2 xs)=='N') && ((vepal 3 xs)=='T') && ((vepal 4 xs)=='A') && ((vepal 5 xs)==' ') && ((vepal 6 xs)=='E') = "3" ++ (makepal xs)
	       | (x=='Q') && ((vepal 0 xs)=='U') && ((vepal 1 xs)=='A') && ((vepal 2 xs)=='R') && ((vepal 3 xs)=='E') && ((vepal 4 xs)=='N') && ((vepal 5 xs)=='T') && ((vepal 6 xs)== 'A') && ((vepal 7 xs)==' ') && ((vepal 8 xs)=='E') = "4" ++ (makepal xs)
	       | (x=='Q') && ((vepal 0 xs)=='U') && ((vepal 1 xs)=='A') && ((vepal 2 xs)=='R') && ((vepal 3 xs)=='E') && ((vepal 4 xs)=='N') && ((vepal 5 xs)=='T') && ((vepal 6 xs)== 'A') = "40 " ++ (makepal xs)
               | (x=='T') && ((vepal 0 xs)=='R') && ((vepal 1 xs)=='I') && ((vepal 2 xs)=='N') && ((vepal 3 xs)=='T') && ((vepal 4 xs)=='A') = "30" ++ (makepal xs)
	       | (x=='C') && ((vepal 0 xs)=='I') && ((vepal 1 xs)=='N') && ((vepal 2 xs)=='Q') && ((vepal 3 xs)=='U') && ((vepal 4 xs)=='E') && ((vepal 5 xs)=='N') && ((vepal 6 xs)== 'T') && ((vepal 7 xs)=='A')= "50 " ++ (makepal xs)
	       | otherwise = "" ++ (makepal xs)

vepal :: Int -> String -> Char
vepal _ [] = '!'
vepal 0 (x:xs) = x
vepal y (x:xs) = vepal (y-1) xs
--------------------------------------------Verifica a funcao do euromilhoes se e valida---------------------------------------------------------------------
euromilhoes :: String -> Bool
euromilhoes [] = False
euromilhoes s@(x:xs) = if (euromilhoesaux(take 5 (words (makepal s)))) && (euromilhoesaux(take 2 (reverse (words (makepal s))))) && ((length (words (makepal s))) == 7) then True
                                    else False
euromilhoesaux :: [String] -> Bool
euromilhoesaux [] = True
euromilhoesaux (x:xs) = if ((euroaux x xs) == True) then (euromilhoesaux xs)
                        else False

euroaux :: String -> [String] -> Bool
euroaux _ [] = True
euroaux s (x:xs) = if (s /= x) then (euroaux s xs)
                   else False

---------------------------Execucao da funcao: euromilhoes " quarenta e sete oito nove dez doze um dois" os ultimos dois numeros sao as estrelas-------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------Questao 4----------------------------------------------------------------------------------------
maiusculas :: String -> String
maiusculas [] = []
maiusculas (x:xs) = toUpper x : maiusculas xs
--filtra e preciso fazer como deve ser
filtra :: String -> String
filtra [] = []
filtra a = filter isAlpha a

assemblar :: String -> String
assemblar a = filtra(maiusculas a)
-------------------------------------------------------------------------------------------------
--

----------------------Cifra de Cesar----------------------------------------------------------------

cesar1 :: Palavra -> Int -> Palavra
cesar1 []  _ = []
cesar1 x 0 = x
cesar1 (x:xs) y = if ( ((ord x + mod y 26) >= 65) && ((ord x + mod y 26) <= 90) )  then (chr (ord x + mod y 26)) : cesar1 xs y
                  else  (chr ((ord x + mod y 26)-26)) : cesar1 xs y

cifrar :: Palavra -> Int -> Palavra
cifrar a@(x:xs) y = cesar1 (assemblar a) y

-----------------------Decifra a cifra de cesar-----------------------------------------------

decifrar :: Palavra -> Int -> Palavra
decifrar a@(x:xs) y = decif (assemblar a) y 


decif :: Palavra -> Int -> Palavra
decif [] _ = []
decif x 0 = x
decif (x:xs) y = if ( ((ord x - mod y 26) >= 65) && ((ord x - mod y 26) <= 90) )  then (chr (ord x - mod y 26)) : decif xs y
                  else (chr ((ord x - mod y 26)+26)) : decif xs y
-- VIGENERE
--A Funcao a utilizar é a cifrarV. Ex: cifarV "oioioi" "ola"

cifrarV :: String -> String -> String
cifrarV [] _ = []
cifrarV s@(x:xs) c = cifrarVaux (assemblar s) (assemblar c) (assemblar c)

cifrarVaux :: String -> String -> String -> String
cifrarVaux [] _ _ = []
cifrarVaux s@(x:xs) c@(y:ys) d  = if (tail c == "") then cifrar (x:[]) ((ord y)-65) ++ cifrarVaux xs d d
                                  else  cifrar (x:[]) ((ord y)-65) ++ cifrarVaux xs ys d

--Aqui foi desenhada a decifragem de vigenere.
--A Funcao a utilizar é a decifV. Ex: decifV "CTOWZI" "ola"
decifV :: String -> String -> String
decifV [] _ = []
decifV s@(x:xs) c = decifVaux (assemblar s) (assemblar c) (assemblar c)

decifVaux :: String -> String -> String ->String
decifVaux [] _ _ = []
decifVaux s@(x:xs) c@(y:ys) d = if (tail c == "") then decif (x:[]) ((ord y)+65) ++ decifVaux xs d d	
				else decif (x:[]) ((ord y)+65) ++ decifVaux xs ys d



vericFundos :: Int -> Int -> IO Bool
vericFundos afund x = do return(afund>=x)

verifAg :: Int -> Int -> IO Bool
verifAg x meta = do{
                     roll<-dado 20;                                   
                     return(((div (x-10) 2) + roll)>=meta)
                    }

-


--- Geradores ---

gerachave :: IO String
gerachave = do {
	                a1 <- dado (length basepal);
	                a2 <- dado (length basepal);
	                a3 <- dado (length basepal);
	                a4 <- dado (length basepal);
	                a5 <- dado (length basepal);
	                a6 <- dado (length basepal);
                        return  ( (vepal2 a1 basepal) ++" "++ (vepal2 a2 basepal) ++" "++ (vepal2 a3 basepal)  ++" "++ (vepal2 a4 basepal)  ++" "++ (vepal2 a5 basepal)  ++" "++ (vepal2 a6 basepal)  )
                        }


geraeuro = do {
              k <- geraeuroaux;
              if (euromilhoes k) then return (k)
              else geraeuro;
              }


 
geraeuroaux = do { 
                a1 <- dado (length basenum);
	        a2 <- dado (length basenum);
	        a3 <- dado (length basenum);
	        a4 <- dado (length basenum);
	        a5 <- dado (length basenum);
	        a6 <- dado 9;
	        a7 <- dado 9;
                return  ( (vepal2 a1 basenum) ++" "++ (vepal2 a2 basenum) ++" "++ (vepal2 a3 basenum)  ++" "++ (vepal2 a4 basenum)  ++" "++ (vepal2 a5 basenum)  ++" "++ (vepal2 a6 basenum) ++" "++ (vepal2 a7 basenum)  )            
                }
             
vepal2 :: Int -> [String] -> String
vepal2 _ [] = ""
vepal2 0 (x:xs) = x
vepal2 y (x:xs) = vepal2 (y-1) xs
