module viagens where

import Data.Char
 
 --------------------------------------------- Viagens------
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