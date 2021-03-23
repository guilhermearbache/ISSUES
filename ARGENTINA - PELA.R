##### ARGENTINA 2015

library(haven)
library(sjlabelled)

arg_87 <- read_sav ("C:/Users/livia/OneDrive - usp.br/TESE/DATASETS/PELA - NEW/BASEDATOS_ARGENTINA_87/BASEDATOS_ARGENTINA_87.sav")




# ARGENTINA - ESTUDO 87

arg_87 <- dplyr::arrange(arg_87, partido)
arg_87$interview <- rep(tabulate(arg_87$partido), tabulate(arg_87$partido))  # QUANTIDADE DE ENTREVISTADOS POR PARTIDO 

arg_87$party <- sjlabelled::as_label(arg_87$partido) #CRIANDO VARIÁVEL PARA NOME DOS PARTIDOS (MELHOR PARA VISUALIZAR E ATÉ
#VOU USAR ESSA MESMA PARA INSERIR DADOS DE VOTES E SEATS, FICA MAIS FÁCIL DE EVITAR E CONFERIR ERROS)


#arg_87 <- remove_all_labels(arg_87) #SERIA PARA BIND_ROWS POR ENQUANTO ESTOU TRABALHANDO COM ESSE BANCO SOZINHO

#INSERIR DADOS DE VOTE E SEATS

# CONSIDERO SHARES DA COALIZÃO, PORQUE É BASTANTE CONFUSO CONSIDERAR PARTIDO INDIVIDUALMENTE, ALGUNS SEQUER ACHAMOS DADOS
#NA VERDADE MESMO COALIZÃO É CONFUSO EM ALGUNS CASOS (ESTÁ NUMA COALIZÃO NUMA PROVÍNCIA, EM OUTRA NÃO), MAS DOS PARTIDOS
#COM CADEIRA, OU MELHOR, COM ALGUM ENTREVISTADO AQUI, PARECE QUE FOI POSSÍVEL FAZER ISSO. 

# DETALHE: AS COALIZÕES EM OUTRAS ELEIÇÕES DEVEM TER SIDO DIFERENTES, PROVAVELMENTE HÁ ALIANÇAS QUE NEM EXISTIAM. 
#POR ISSO É MELHOR CONCENTRAR TODA ANÁLISE NOS ENTREVISTADOS QUE FAZEM PARTE DA LEGISLATURA 2011-2015 (filtrar por legis)
#A CORRESPONDÊNCIA DE CADA PARTIDO/COALIZÃO JÁ É DIFÍCIL

# VOTES #
arg_87$votes <- 0

arg_87$votes[arg_87$party == "Frente para la Victoria"] <- 52.46 
arg_87$votes[arg_87$party == "UCR"] <- 13.49 
arg_87$votes[arg_87$party == "PJ"] <- 52.46 
arg_87$votes[arg_87$party == "Coalición Cívica"] <- 3.00 
arg_87$votes[arg_87$party == "PRO"] <- 6.92 
arg_87$votes[arg_87$party == "Frente Cívico y Social de Santiago del Estero"] <- 52.46 
arg_87$votes[arg_87$party == "Partido Socialista"] <- 13.52 
arg_87$votes[arg_87$party == "GEN"] <- 13.52 
arg_87$votes[arg_87$party == "Nuevo Encuentro"] <- 13.49 
arg_87$votes[arg_87$party == "Proyecto Sur"] <- 0.16 
arg_87$votes[arg_87$party == "Moviemiento Popular Neuquino"] <- 0.43 
arg_87$votes[arg_87$party == "Demócrata Mendoza"] <- 5.93 
arg_87$votes[arg_87$party == "Libres del Sur"] <- 13.52 
arg_87$votes[arg_87$party == "Renovador de Salta"] <- 52.46 
arg_87$votes[arg_87$party == "Partido Federal Fueguino"] <- 0
arg_87$votes[arg_87$party == "Salta Somos Todos"] <- 6.92 
arg_87$votes[arg_87$party == "Demócrata Progresista"] <- 13.49 
arg_87$votes[arg_87$party == "Movimiento Popular Fuegino"] <- 0.07 
arg_87$votes[arg_87$party == "MIJD"] <- 0.29 
arg_87$votes[arg_87$party == "Unión para Todos"] <- 3.00 
arg_87$votes[arg_87$party == "Frente Cívico-Partido Nuevo"] <- 13.52 

arg_87$votes[arg_87$party == "Partido Bloquista de San Juan"] <- 52.46 
arg_87$votes[arg_87$party == "Partido de la Concertación-Forja"] <- 52.46 
arg_87$votes[arg_87$party == "Unión Celeste y Blanca"] <- 13.49 
arg_87$votes[arg_87$party == "PJ Disidente"] <- 52.46 
arg_87$votes[arg_87$party == "Partido Renovador de la Concordia"] <- 52.46 


  
##### SEATS ##### 

#Aqui são SEATS GAINED, não o total!
arg_87$seats <- 0
arg_87$seats[arg_87$party == "Frente para la Victoria"] <- 86
arg_87$seats[arg_87$party == "UCR"] <- 16
arg_87$seats[arg_87$party == "PJ"] <- 86
arg_87$seats[arg_87$party == "Coalición Cívica"] <- 1
arg_87$seats[arg_87$party == "PRO"] <- 5
arg_87$seats[arg_87$party == "Frente Cívico y Social de Santiago del Estero"] <- 86
arg_87$seats[arg_87$party == "Partido Socialista"] <- 14
arg_87$seats[arg_87$party == "GEN"] <- 14
arg_87$seats[arg_87$party == "Nuevo Encuentro"] <- 16
arg_87$seats[arg_87$party == "Proyecto Sur"] <- 0
arg_87$seats[arg_87$party == "Moviemiento Popular Neuquino"] <- 1
arg_87$seats[arg_87$party == "Demócrata Mendoza"] <- 6
arg_87$seats[arg_87$party == "Libres del Sur"] <- 14
arg_87$seats[arg_87$party == "Renovador de Salta"] <- 86
arg_87$seats[arg_87$party == "Partido Federal Fueguino"] <- 0
arg_87$seats[arg_87$party == "Salta Somos Todos"] <- 5
arg_87$seats[arg_87$party == "Demócrata Progresista"] <- 16
arg_87$seats[arg_87$party == "Movimiento Popular Fuegino"] <- 1
arg_87$seats[arg_87$party == "MIJD"] <- 0
arg_87$seats[arg_87$party == "Unión para Todos"] <- 1
arg_87$seats[arg_87$party == "Frente Cívico-Partido Nuevo"] <- 14

arg_87$seats[arg_87$party == "Partido Bloquista de San Juan"] <- 86
arg_87$seats[arg_87$party == "Partido de la Concertación-Forja"] <- 86
arg_87$seats[arg_87$party == "Unión Celeste y Blanca"] <- 16
arg_87$seats[arg_87$party == "PJ Disidente"] <- 86
arg_87$seats[arg_87$party == "Partido Renovador de la Concordia"] <- 86


#### E O PJ DISIDENTE?


# RODAR COM O RESTO 



#EMD TOTAL - FAZER COM SEPARADO DE 11 A 15 E 09 A 13, E COM TODOS - E FAZER SEM VOTES SHARES MESMO, COM A AMOSTRA DOS ENTREVISTADOS
#JÁ QUE OS PARTIDOS SÃO TÃO CONFUSOS - COMO INSERIR ISSO COM O RESTO? TEREI QUE FAZER FILTROS 




##### MULTILEVEL #####
#GROUP BY PARTIDO PARA INSERIR DADOS NO LAPOP? E DE QUEM, PARTIDO PRINCIPAL OU COALIZÃO TODA? AMBOS

