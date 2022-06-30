module Aventuras where

import Char
import IO
import Tipos
import Funcoes


menuaventura1 ::  Agente -> Int -> Int -> Int -> Char -> Char -> String ->IO Agente
menuaventura1 (A af ad ai ac afund) c d x a1 a2 des = do{
                       putStr (des);
                       playerAg (A af ad ai ac afund);
                       op<-leropcao (a1:a2:"x0");
                       if (op=='0') then return (A af ad ai ac afund)
                       else do {
                                if (op==a1) then do {res<-abilidades ac c;
                                                      if res then do return (A af ad ai ac afund);
                                                           else do putStrLn "Falhou!"; getChar; menuaventura1 (A af ad ai ac afund) c d x a1 a2 des;
                                                     }
                                else if (op == a2) then do {res<-abilidades ad d; 
                                                            if res then do return (A af ad ai ac afund);
                                                                 else do putStrLn "Falhou!"; getChar; menuaventura1 (A af ad ai ac afund) c d x a1 a2 des;
                                                             }
                                     else do {res<-verificarFundos afund (x+99);
                                              if res then do return (A af ad ai ac (afund-(x+99)));
                                                    else do putStrLn "Nao tem fundos suficientes!"; getChar; menuaventura1 (A af ad ai ac afund) c d x a1 a2 des;
                                             }
                               }
                       
                       
                      }

-------------------------------------------------------------------------------------------------------
des_menuAventura1 c d x= 	" ----------------Menu Aventura1-------------------------------------- \n " ++
                	 	"|E uma noite fria, voce encontra-se em frente ao edificio onde o      |\n " ++
			 	"|primeiro suspeito trabalha e tem guardada uma parte da chave. Voce   |\n " ++
                                "|consegue chegar ao gabinete dele.                                    |\n " ++
                	 	"|Tem 3 hipoteses:                                                     |\n " ++
		 	 	"|                                                                     |\n " ++
		 	 	"|                                                                     |\n " ++
                	 	"|Convencer a empregada do escritorio a abrir-lhe a porta(Carisma "++(show c)++").c|\n " ++
                	 	"|Esgueirar-se sem o guarda ver e arrombar a porta..(Destreza "++(show d)++").....d|\n " ++
		         	"|Subornar o guarda para ele lhe dar acesso...........(Fundos "++(show x)++")....x|\n " ++
			 	"|                                                                     |\n " ++
			 	"|                                                                     |\n " ++
			 	"|Terminar...........................................................0 |\n " ++
               		 	" ------------------------------------------------------------------- \n\n\n"

des_menuAventura2 i d x = 	" ----------------Menu Aventura2------------------------------------- \n " ++
                		"|Voce conseguiu entrar no laboratorio nuclear \"XPTO factory\" e        |\n " ++
				"|descobre o computador onde esta guardada uma parte da chave.         |\n " ++
                                "|Tem 3 hipoteses:                                                     |\n " ++
                		"|                                                                     |\n " ++
				"|                                                                     |\n " ++
                		"|Tentar descodificar a pass...................(Inteligencia "++(show i)++").......i|\n " ++
                		"|Esconder-se e ler a pass de um funcionario..........(Destreza "++(show d)++")...d|\n " ++
				"|Adquirir um software de desencriptacao...........(Fundos "++(show x)++").......x|\n " ++
				"|                                                                     |\n " ++
				"|                                                                     |\n " ++
				"|Terminar...........................................................0 |\n " ++
                		" -------------------------------------------------------------------- \n\n\n"

des_menuAventura3 f c x =	" ----------------Menu Aventura3-------------------------------------- \n " ++
                		"|Um empresario, suspeito, convida-o para jantar em sua casa, ele     |\n " ++
				"|possui uma outra parte da chave da loteria, de imediato voce aceita |\n " ++
                                "|o convite. Ao chegar a casa dele descobre que ele tem um seguranca. |\n " ++
                		"|Ao chegar a casa dele descobre que ele tem um seguranca.            |\n " ++
                	 	"|Tem 3 hipoteses:                                                    |\n " ++
                                "|                                                                    |\n " ++
				"|                                                                    |\n " ++
                		"|Tentar derrubar os 2 ..........................(Forca "++(show f)++")...........f|\n " ++
                		"|Convencer ao dono da casa a entregar-lhe a chave...(Carisma "++(show c)++")....c|\n " ++
				"|Subornar o seguranca para roubar a chave...........(Fundos "++(show x)++")....x|\n " ++
				"|                                                                    |\n " ++
				"|                                                                    |\n " ++
				"|Terminar...........................................................0|\n " ++
                		" -------------------------------------------------------------------- \n\n\n"

des_menuAventura4 c i x= 	" ----------------Menu Aventura4-------------------------------------- \n " ++
                		"|Voce recebeu informacoes que outra parte da chave esta numa casa    |\n " ++
				"|abandonada. Voce dirige-se ate la e encontra 3 caes de guarda.      |\n " ++
                	 	"|Tem 3 hipoteses:                                                    |\n " ++
                		"|                                                                    |\n " ++
				"|                                                                    |\n " ++
                		"|Tentar acalmar os caes........................(Carisma "++(show c)++").........c|\n " ++
                		"|Tentar criar uma distraccao para os caes.......(Inteligencia "++(show i)++")...i|\n " ++
				"|Tentar comprar-lhes comida para os entreter..........(Fundos "++(show x)++")..x|\n " ++
				"|                                                                    |\n " ++
				"|                                                                    |\n " ++
				"|Terminar...........................................................0|\n " ++
               	 		" -------------------------------------------------------------------- \n\n\n"

des_menuAventura5 c i x = 	" ----------------Menu Aventura5-------------------------------------- \n " ++
                		"|Voce recebeu informacoes que outra parte da chave esta numa casa    |\n " ++
				"|protegida. Voce dirige-se ate la e encontra 3 segurancas.           |\n " ++
                	 	"|Tem 3 hipoteses:                                                    |\n " ++
                		"|                                                                    |\n " ++
				"|                                                                    |\n " ++
                		"|Tentar convencer os segurancas a deixa-lo entrar.....(Carisma "++(show c)++")..c|\n " ++
               			"|Tentar criar uma distraccao para os segurancas...(Inteligencia "++(show i)++").i|\n " ++
				"|Tentar subornar os segurancas........................(Fundos "++(show x)++")..x|\n " ++
				"|                                                                    |\n " ++
				"|                                                                    |\n " ++
				"|Terminar...........................................................0|\n " ++
                		" -------------------------------------------------------------------- \n\n\n"

des_menuAventura6 c i x = 	" ----------------Menu Aventura6-------------------------------------- \n " ++
                		"|Voce esta no meio de um hotel para comprar uma parte da chave       |\n " ++
				"|encontra-se com o seu intermediario.                                |\n " ++
                	 	"|Tem 3 hipoteses:                                                    |\n " ++
                		"|                                                                    |\n " ++
				"|                                                                    |\n " ++
                		"|Conversar muito com ele e tenta obter a chave sem pagar.(Carisma"++(show c)++")c|\n " ++
                		"|Tentar embebeda-lo para ele dizer qual e a chave..(Inteligencia "++(show i)++")i|\n " ++
				"|Comprar a chave......................................(Fundos "++(show x)++")..x|\n " ++
				"|                                                                    |\n " ++
				"|                                                                    |\n " ++
				"|Terminar...........................................................0|\n " ++
                		" -------------------------------------------------------------------- \n\n\n"

menuFinal pass text =     " -----------------------------Menu Final----------------------------- \n " ++
                "|Parabens. Ja tem todas as mensagens.                                |\n " ++
                "|A mensagem e "++(show pass)++" e a chave e "++(show text)++" |\n " ++
		"|O que deseja fazer agora:                                           |\n " ++
                "|                                                                    |\n " ++
		"|                                                                    |\n " ++
                "|Decifrar usando a cifra de vigenere................................1|\n " ++
                "|Comparar com a frase guardada internamente.........................2|\n " ++
		"|                                                                    |\n " ++
		"|                                                                    |\n " ++
		"|                                                                    |\n " ++
		"|Terminar...........................................................0|\n " ++
                " -------------------------------------------------------------------- \n\n\n"

leropcao range = do {x <-lerstr "\nEscolha a opcao: ";
		    (if (elem (head x)range)
		     then return (head x)
		     else do {putStr "opcao invalida\n";
			      leropcao range
                             }  
                    ) 
                    }
leropcao1 range = do {x <- lerstr "\nPrima 0 para voltar ao menu: ";
                     (if (elem (head x)range)
                      then return (head x)
                      else do {putStr "Opcao invalida\n";
                               leropcao1 range
                              }
                     )
                     }

lerstr msg = do putStr msg
	        getLine



--funcfinal :: String -> String -> IO String
funcfinal pass key euro = do {
                             putStrLn (menuFinal pass key);
	                     op<-leropcao "01";
                             if (op=='1') then do { 
                                                  putStr (menu_chave (euro)); 
                                                  }
                             else return() 
                             }

menu_chave key =  " -----------------------------Menu Final----------------------------- \n " ++
                  "|                           !!!Parabens!!!                           |\n " ++
	  	  "|                                                                    |\n " ++
                  "|                     A chave do euromilhoes e "++(show key)++"      |\n " ++
		  "|                                                                    |\n " ++
                  " -------------------------------------------------------------------- \n\n\n"
