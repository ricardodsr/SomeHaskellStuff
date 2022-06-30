module Rpg where

import Char
import Funcoes
import Tipos
import Aventuras

s :: String
s = "oi"

eurokey :: String
eurokey = "SEIS NOVE TREZE QUATORZE TRINTA E CINCO TRES QUATRO"

cryptkey :: String
cryptkey = "OFFICE XPTO HOME PITBULL PROSSEGUR HILTON"

password :: String
password = cifrarV cryptkey eurokey

---Interface---------
menu :: IO ()
menu = do {
           putStrLn mainmenu;
           op<-leropcao "012";
           if (op=='1') then do {
                                 p<-playerskill;
                                 a<-atributos;
                                 b<-atributos;
                                 x<-dado 100;
                                 np<-menuaventura1 p a b x 'c' 'd' (des_menuAventura1 a b (99+x));
                                 a<-atributos;
                                 b<-atributos;
                                 x<-dado 100;
                                 np<-menuaventura1 np a b x 'i' 'd' (des_menuAventura2 a b (99+x));
                                 a<-atributos;
                                 b<-atributos;
                                 x<-dado 100;
                                 np<-menuaventura1 np a b x 'f' 'c' (des_menuAventura3 a b (99+x));
                                 a<-atributos;
                                 b<-atributos;
                                 x<-dado 100;
                                 np<-menuaventura1 np a b x 'c' 'i' (des_menuAventura4 a b (99+x));
                                 a<-atributos;
                                 b<-atributos;
                                 x<-dado 100;
                                 np<-menuaventura1 np a b x 'c' 'i' (des_menuAventura5 a b (99+x));
                                 a<-atributos;
                                 b<-atributos;
                                 x<-dado 100;
                                 np<-menuaventura1 np a b x 'c' 'i' (des_menuAventura6 a b (99+x));
                                 go<-(funcfinal password cryptkey eurokey);
                                 return ()
                                }
           else if (op=='2') then do {
                                      putStrLn menuAutores;
				      op<-leropcao "012";
				      if (op=='1') then do { putStrLn menuNome;  
   							     op2<-leropcao "0";
							     if (op2=='0') then do { menu; }
                                                             else do { return() } }
                                      else if (op=='2') then do { putStrLn menuEmail; 
                                                                  op2<-leropcao "0";
							          if (op2=='0') then do { menu; }
                                                                  else do { return() } }
                                      else if (op=='0') then do { menu; }
                                      else do { return() }                     
                                     }
                else do {
                         return ()
                        }
          }
----------------------------------

     
mainmenu = " ----------------------------------------\n"++
           "|                                        |\n"++
           "|Novo Jogo..............................1|\n"++
           "|Autores................................2|\n"++
           "|Sair...................................0|\n"++
           " ----------------------------------------\n"

menuAutores =  " -------------------------Autores-------------------------\n "  ++
               "|Nomes dos autores.......................................1|\n " ++
               "|Contactos...............................................2|\n " ++
               "|                                                         |\n " ++
               "|Voltar ao menu Principal................................0|\n " ++
               " ---------------------------------------------------------\n\n\n "

menuNome =   " --------------Autores deste trabalho---------------------------\n "  ++
             "|Nome: Ricardo Dovichi de Sousa Rouco..............Numero: 39502|\n " ++
             "|Nome: Joao Pedro Brito Cicio de Carvalho..........Numero: 43137|\n " ++
             "|                                                               |\n " ++
	     "|                                                               |\n " ++
	     "|Voltar ao menu Principal......................................0|\n " ++
             " ---------------------------------------------------------------\n\n\n "
        
menuEmail = "  ----------------------Email para contactos-------------------- \n " ++
            "|Nome: Ricardo ...................Email: ricardo.rouco@gmail.com|\n " ++
            "|Nome: Joao ........................Email: wick3d.sick@gmail.com|\n " ++
            "|                                                               |\n " ++
	    "|                                                               |\n " ++
	    "|Voltar ao menu Principal......................................0|\n " ++	
            " ---------------------------------------------------------------\n\n\n " 



