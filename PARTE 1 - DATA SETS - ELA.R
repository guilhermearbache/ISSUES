
##### PACKAGES #####

library(haven)
library(dplyr)
library(sjlabelled)

##### DOWNLOAD DATASETS #####
#1.1. Lendo todos os arquivos SPSS, cada um para um banco de dados respectivo no R.

setwd("C:/Users/livia/OneDrive - usp.br/TESE/LATAM/PELA/5TH WAVE STYLE")

filenames <- list.files()
filelist <- lapply(filenames, read_spss)


# Agora, precisamos atribuir um nome a esses bancos antes de juntá-los. Mas antes, para facilitar, 
#vamos dar uma "limpada" nesses nomes:

filenames <- gsub(".sav", "", filenames)
filenames <- gsub(".SAV", "", filenames)
filenames <- gsub("-", "", filenames)
filenames <- tolower(filenames)

#Agora sim, criando os nomes para cada objeto, cada banco de dados original:
names(filelist) <- c(filenames)


#Agora, carregando todos para data.frames no R:

invisible(lapply(names(filelist), function(x) assign(x,filelist[[x]],envir=.GlobalEnv)))

arg_87 <- read_sav ("C:/Users/livia/OneDrive - usp.br/TESE/DATASETS/PELA - NEW/BASEDATOS_ARGENTINA_87/BASEDATOS_ARGENTINA_87.sav")


##### BRASIL #####
#O banco referente ao Brasil teve que ser modificado antes de adicionar, porque tinha algumas VARIÁVEIS
#discrepantes dos outros (tanto em seus nomes quanto na classe).

bra_sub <- read_spss ("C:/Users/livia/OneDrive - usp.br/TESE/LATAM/PELA/da75bra.sav")

bra_sub <- bra_sub %>% 
  dplyr::rename (partido = Partido, nestu = N.Estudo) %>%
  select (nestu, partido, VAL1,	VAL2,	
          PRIV1,	PRIV2, 	EM1,	GP1_1, GP1_2, dplyr::starts_with("ROES"), 
          dplyr::starts_with ("ID"), -contains ("IDLI")) 

bra_sub$id <- "12"

#OUTRAS ALTERAÇÕES NOS NOMES

names (bra_sub) <- gsub("_a", "01", names(bra_sub)) 
names (bra_sub) <- gsub("_b", "02", names(bra_sub)) 
names (bra_sub) <- gsub("_c", "03", names(bra_sub)) 
names (bra_sub) <- gsub("_d", "04", names(bra_sub)) 
names (bra_sub) <- gsub("_e", "05", names(bra_sub)) 
names (bra_sub) <- gsub("_f", "06", names(bra_sub)) 
names (bra_sub) <- gsub("_g", "07", names(bra_sub)) 
names (bra_sub) <- gsub("_h", "08", names(bra_sub)) 
names (bra_sub) <- gsub("_i", "09", names(bra_sub)) 
names (bra_sub) <- gsub("_j", "10", names(bra_sub)) 
names (bra_sub) <- gsub("_k", "11", names(bra_sub))
names (bra_sub) <- gsub("_l", "12", names(bra_sub)) 
names (bra_sub) <- gsub("_m", "13", names(bra_sub)) 
names (bra_sub) <- gsub("_n", "14", names(bra_sub)) 

#Para adequar algumas VARIÁVEIS ainda falta fazer o seguinte:

names (bra_sub) <- gsub("_", "0", names(bra_sub)) 

##### GUA - ADAPTAÇÕES #####

#Banco da Guatemala tem o nome da variável identificadora 
#de partido diferente do resto. 
#Além disso partidos GANA E UNE ENTRARAM COMO COALIZÃO, então vou juntar como um só partido, porque não temos resultados de votos
# separados entre os dois (se quiser analisar, por exemplo, a congruência agregada, por partido, sem a etapa "votos", 
#talvez seja melhor separar de novo (só tirar essa linha de código e inserir o código acima para mudar o nome da variável):

da85gua$partido <- da85gua$PP
da85gua$partido[da85gua$partido==6] <- 4


##### VOTES & SEATS #####

# CRIANDO VARIÁVEIS PARA VOTES SHARES, SEATS, Nº ENTREVISTADOS, E WEIGHTS DE VOTE SHARES E DE SEATS:

## CRIO UMA VARIÁVEL COM Nº DE ENTREVISTADOS POR PARTIDO - achar jeito melhor para isso!

### e antes preciso fazer um "arrange":

da74hon<- arrange(da74hon, partido)
da76uru<- arrange(da76uru, partido)
da77chi<- arrange(da77chi, partido)
da78cr<- arrange(da78cr, partido)
da80pe<- arrange(da80pe, partido)
da81bo<- arrange(da81bo, partido)
da82rdom<- arrange(da82rdom, partido)
da83co<- arrange(da83co, partido)
da84pe<- arrange(da84pe, partido)
da85gua<- arrange(da85gua, partido)
da86nica<- arrange(da86nica, partido)

da74hon$interview <- rep(tabulate(da74hon$partido), tabulate(da74hon$partido))
da76uru$interview <- rep(tabulate(da76uru$partido), tabulate(da76uru$partido))
da77chi$interview <- rep(tabulate(da77chi$partido), tabulate(da77chi$partido))
da78cr$interview <- rep(tabulate(da78cr$partido), tabulate(da78cr$partido))
da80pe$interview <- rep(tabulate(da80pe$partido), tabulate(da80pe$partido))
da81bo$interview <- rep(tabulate(da81bo$partido), tabulate(da81bo$partido))
da82rdom$interview <- rep(tabulate(da82rdom$partido), tabulate(da82rdom$partido))
da83co$interview <- rep(tabulate(da83co$partido), tabulate(da83co$partido))
da84pe$interview <- rep(tabulate(da84pe$partido), tabulate(da84pe$partido))
da85gua$interview <- rep(tabulate(da85gua$partido), tabulate(da85gua$partido))
da86nica$interview <- rep(tabulate(da86nica$partido), tabulate(da86nica$partido))






bra_sub<- arrange(bra_sub, partido)
bra_sub$interview <- rep(tabulate(bra_sub$partido), tabulate(bra_sub$partido))

### TIVE QUE RETIRAR LABELS PORQUE ESTAVA DANDO ERRO NO BIND_ROWS!
da74hon <- remove_all_labels(da74hon)
da76uru <- remove_all_labels(da76uru)
da77chi<- remove_all_labels(da77chi)
da78cr <- remove_all_labels(da78cr) 
da80pe <- remove_all_labels(da80pe) 
da81bo <- remove_all_labels(da81bo)
da82rdom <- remove_all_labels(da82rdom)
da83co<- remove_all_labels(da83co)
da84pe <- remove_all_labels(da84pe)
da85gua<- remove_all_labels(da85gua)
da86nica <- remove_all_labels(da86nica)


# ARGENTINA - ESTUDO 87
# - FIZ ALTERAÇÕES EM UM SCRIPT SEPARADO 

##### BIND #####

ela_almost <- bind_rows(da74hon, da76uru, da77chi, da78cr, da80pe, da81bo, 
                        da82rdom, da83co, da84pe, da85gua, da86nica, arg_87, .id = "id")

# Já criei uma variável "id" para poder identificar facilmente cada país, cada banco, numa sequência
#numérica, fica mais fácil de criar "cname" assim. 


# SELECIONANDO VARIÁVEIS 

ela_almost <- ela_almost %>%
  select(nestu, id, partido, interview, VAL1, VAL2,	
         PRIV1,	PRIV2, 	EM1,	starts_with("GP"), starts_with("ROES"),  -contains ("ROES2"),
         starts_with ("ID"), -contains ("IDLI"))

ela <- bind_rows(ela_almost, bra_sub)

#Ainda estou mantendo uma coluna para nº Estudo, outra para id de cada banco (na ordem que foram baixados)

## PODERIA TRANSFORMAR O ID NOS NOMES DAS FILENAMES, DEPOIS TIRAR O "DA", ETC. PARA FICAR PRÓXIMO DO NOME 
#DOS PAÍSES COM 3 LETRAS. ALGUNS IA CORRESPONDER, MAS DEPOIS IA TER TODO TRABALHO DE PADRONIZAR O RESTO DOS PAÍSES
#MAIS FÁCIL FAZER DIRETO AGORA, CRIANDO UMA LISTA CORRESPONDENTE A ID:

#OBS- BRASIL FICOU LÁ NO FINAL, COMO 12 (PORQUE ESTAVA FORA DA LISTA INICIAL):

oldvalues <- c(1:13)
newvalues <- factor (c("HND", "URY", "CHL", "CRI", "PER", "BOL", "DOM", "COL", "PER", "GTM", "NIC", "ARG", "BRA" ))  

ela$cname <- newvalues[match(ela$id, oldvalues)]

### ANTES DE TERMINAR, VOU RESOLVER ALGUNS MISSINGS - PARA VAL1 E VAL2, ROES. OUTRAS VARIÁVEIS
# QUE TALVEZ TENHAM MISSING EM 8 E 9, POR EXEMPLO, PODEM ESTAR DE FORA, AÍ PRECISA INCLUIR ELAS NESSE PRIMEIRO CÓDIGO:

ela <- ela %>%
  mutate_at(.vars = vars(ROES101:ROES109), 
            .funs = funs(ifelse(. == 9, NA, .)))

ela <- ela %>%
  mutate_at(.vars = vars(ROES101:ROES109), 
            .funs = funs(ifelse(. == 8, NA, .)))


#OUTRAS VARIÁVEIS, COMO VAL1 E VAL2, TEM MISSINGS na casa de 90:

ela <- na_if(ela, 96)
ela <- na_if(ela, 97)
ela <- na_if(ela, 98)
ela <- na_if(ela, 99)


save(ela, file = "C:/Users/livia/OneDrive - usp.br/TESE/LATAM/elanew.RData")


### CRIANDO VARIÁVEL DE VOTOS (VIA EXCEL)
write.csv2(ela, file = "ela_putvotes.csv")




