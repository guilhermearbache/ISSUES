library(haven)
library(tidyverse)
library(dplyr)

setwd("C:/Users/livia/Desktop/ISSUES/")

lapop <- read_dta("Lapop_merge_2004-2018.dta") 


library(sjlabelled)

lap <- remove_all_labels(lapop)

# Tiro "Ros7" que não tem em nenhum banco a ser utilizado do PELA

## SELECIONANDO VARIÁVEIS DE INTERESSE: 

lap <- lap  %>% filter(pais < 22 & year<2017 & year > 2007) %>%
  select (pais, year,  starts_with("ros"), - dplyr::contains("ros7"),d6,q1, q2, q3c, q10, ed, l1, a4, tamano,
          starts_with("gi"), starts_with("w14"), vb2, vb2v,starts_with("vb3"), vb20,vb5,vb6,vb8) %>%
  mutate(cname = recode(pais, `1` = "MEX", `2` = "GTM", `3` = "SLV", `4` = "HND", `5` = "NIC", `6` ="CRI",      # CRIANDO CNAME 
                        `7` = "PAN", `8` = "COL", `9` ="ECU", `10` = "BOL", `11` = "PER", `12`= "PRY", 
                        `13` = "CHL", `14` = "URY", `15` = "BRA", `16` = "VEN", `17` = "ARG", `21` = "DOM")) 

#l1b estava na seleção anterior, é a classificação Lib-Conservador, ao invés de Left-Right, mas nenhum aqui da amostra
#tem esse dado. Qq coisa retornar ali no select

#ROS7 tirei porque não parece ter em quase nenhum banco, mas também tem no ELA! Daria para usar se for correspondente


##### GRAND-MERGE #####

#AQUI FICA QUASE IGUAL A VERSÃO ANTERIOR (LASPP), ALGUMAS INCLUSÕES DE PAÍSES QUE SÓ TINHA 2010 OU 2012, E 
#AGORA TAMBÉM INCLUÍ OS NOVOS (ECU, SLV, MEX, PAN, PRY)


##### FILTER PAÍS-ANO #####

lap <- lap %>%
  filter (cname == "COL" & year == 2012 | cname == "CRI" & year == 2012 
         |cname == "GTM" & year == 2012 | cname == "HND" & year == 2010 | cname == "HND" & year == 2012
         |cname == "NIC" & year == 2012 | cname == "PER" & year == 2010 
         |cname == "PER" & year == 2012 | cname == "DOM" & year == 2010 | cname == "DOM" & year == 2012
         |cname == "URY" & year == 2010 | cname == "URY" & year == 2012
         |cname == "ECU" & year == 2012 | cname == "SLV" & year == 2012   # PAÍSES INSERIDOS NA SEGUNDA VERSÃO (DEPOIS DE LASPP)
         |cname == "MEX" & year == 2014 |cname == "PAN" & year == 2014  
         |cname == "PRY" & year == 2014 |cname == "PRY" & year == 2016 )

#Mesmo para EMD, não é possível filitrar voter e non-voter se não tiver pergunta "voted" (vb2)
#que se refira À ELEIÇÃO DO mesmo ano a que se refere a Legislatura que vamos usar para ELA. 

#### COLOMBIA 2010 tirei porque se refere à eleição de 2006 

#CRI 2010 tirei porque se refere à eleição de 2006 também

#HND só estava incluído o 2010 na versão anterior (talvez o banco de 2012 ainda não estivesse liberado)
#Agora incluí 2010 E 2012 -  AMBOS TEM TUDO, voto referente a 2009 em ambos. 
#SÓ ROS5 QUE TEM APENAS EM 2010 como em todos bancos

#GUATEMALA SÓ DEIXO O 2012 mesmo, como na versão anterior, porque a Legislatura é 2012-2016. 

#NIC também deixei 2012 apenas, não incluí 2010 porque a eleição não tinha ido ainda

#REP. DOMINICANA é um problema, a eleição de Presidente foi 2008, Legislatura 2010-2016 (na verdade problema
#semelhante pode ter em outros países em que mesmo sendo no mesmo ano a eleição para Deputado pode ter sido 
#em DATA DIFERENTE da Presidencial, então a pessoa pode ter comparecido em uma e não outra)

#URUGUAY tem ambas para 2009 mesmo  

#EL SALVADOR tanto em 2010 quanto 2012 perguntava sobre eleição presidencial de 2009 (o PELA que usamos é de 2012-2015)
#Mas o banco de 2012 tem pergunta sobre eleição legislativa! 
#VB15. ¿Votó usted en las pasadas elecciones legislativas y de Concejos Municipales de marzo de este año (2012)? 

#PANAMA 2014 É ÚTIL PARA O MULTILEVEL LEGISLATIVO (tem "votaria")! Deixei aqui por enquanto depois 
#para EMD tiro ele. Para o EMD vamos usar o 2016 (inserido avulso, abaixo)



###### PAÍSES AVULSOS ######

##### DA VERSÃO ANTIGA #####

### INSERIR PAÍSES QUE não TINHAM 2010 e 2012 NO LAPOP MERGED:  
#MESMO CÓDIGO QUE JÁ ESTAVA NA VERSÃO DA LASPP - ESTAVA NO SCRIPT "PARTE 2":

## 2010:

# BOLIVIA
lapbol <- read_dta ("C:/Users/livia/OneDrive - usp.br/TESE/LATAM/LASPP/1748144313Bolivia_LAPOP_AmericasBarometer 2010.dta")
lapbol$cname <- "BOL"
lapbol$year <- 2010

lapbol <- lapbol%>%
  select (cname, year, l1, 
          starts_with("ros"), - dplyr::contains("ros7"), d6, starts_with("gi"), starts_with("w14"), starts_with("vb")) 

#CHILE:

lapchl10 <- read_dta ("C:/Users/livia/OneDrive - usp.br/TESE/LATAM/LASPP/1217681709Chile_LAPOP_AmericasBarometer 2010.dta")

lapchl10$cname <- "CHL"
lapchl10$year <- 2010

lapchl10 <- lapchl10 %>% 
  select (cname, year, l1, 
          starts_with("ros"), - dplyr::contains("ros7"), d6, starts_with("gi"), starts_with("w14"), starts_with("vb")) 


# 2012 - não estava na versão LASPP, incluir:

lapchl12 <- read_dta ("http://datasets.americasbarometer.org/database/files/420502678Chile%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta")

lapchl12$cname <- "CHL"
lapchl12$year <- 2012

lapchl12 <- lapchl12 %>% 
  select (cname, year, l1, 
          starts_with("ros"), - dplyr::contains("ros7"), d6, starts_with("gi"), starts_with("w14"), starts_with("vb")) 


## 2012 - BOLIVIA:

lapbol12 <- read_dta ("C:/Users/livia/OneDrive - usp.br/TESE/LATAM/LASPP/Bolivia LAPOP 2012 Rev1_W.dta")
lapbol12$cname <- "BOL"
lapbol12$year <- 2012

lapbol12 <- lapbol12 %>% 
  select (cname, year, l1, 
          starts_with("ros"), - dplyr::contains("ros7"), d6, starts_with("gi"), starts_with("w14"), starts_with("vb"))

### BRASIL 2010 - ATÉ EXISTIA NO MERGED, MAS POR ALGUM MOTIVO não TINHA DADOS PARA A VARIÁVEL D6(same_sex marriage).
#POR ISSO PEGUEI A VERSÃO ESPECÍFICA, COMO NESSES OUTROS PAÍSES ACIMA, E AGORA FAZEMOS OS MESMOS PROCEDIMENTOS:

lapbra <- read_dta ("C:/Users/livia/OneDrive - usp.br/TESE/LATAM/LASPP/Brazil_LAPOP_AmericasBarometer 2010 data set  approved v4.dta")
lapbra$cname <- "BRA"
lapbra$year <- 2010

lapbra <- lapbra%>%
  select (cname, year, l1, 
          starts_with("ros"), - dplyr::contains("ros7"), d6, starts_with("gi"), starts_with("w14"), starts_with("vb"))



##### BANCOS NOVOS #####

# AQUI ADICIONO OS QUE NÃO ESTAVAM NA ANÁLISE EM VERSÕES ANTERIORES - LASPP, ETC. 
# E QUE NÃO ESTÃO NO MERGED TAMBÉM

# PAÍSES NÃO INCLUÍDOS ANTES: ARGENTINA, ECUADOR, EL SALVADOR, MEXICO, PANAMA E PARAGUAY

### DESSES NÃO TEM NO MERGED: ARGENTINA (2012) E PANAMA (2016)


lap_arg12 <- read_dta (url("http://datasets.americasbarometer.org/database/files/1240357522Argentina%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta"))



lap_arg12 <- lap_arg12 %>%
  select (pais, year, q1, q2, q3c, q10new, ed, l1, a4, 
          starts_with("ros"), d6, starts_with("gi"), starts_with("w14"), vb1:vb3) 

lap_arg12$cname <- "ARG"




# PANAMA 2016: 
lap_pan16 <- read_dta (url("http://datasets.americasbarometer.org/database/files/2078678947Panama%20LAPOP%20AmericasBarometer%202017%20V1.0_W.dta"))

lap_pan16$year <- 2016


lap_pan16 <- lap_pan16 %>%
  select (pais, year, q1, q2, q3c, q10new, ed, l1, a4, 
          starts_with("ros"), d6, starts_with("gi"), starts_with("w14"), vb1:vb3n) 

lap_pan16$cname <- "PAN"


##### CONTINUAR DAQUI #####

##### BIND ####


lap <- remove_all_labels(lap)
lapbol <- remove_all_labels(lapbol)
lapbol12 <- remove_all_labels(lapbol12)
lapbra <- remove_all_labels(lapbra)
lapchl10 <- remove_all_labels(lapchl10)
lapchl12 <- remove_all_labels(lapchl12)
lap_arg12 <- remove_all_labels(lap_arg12)
lap_pan16 <- remove_all_labels(lap_pan16)

lapnew <- bind_rows(lap,lapbol, lapbol12,lapbra, lapchl10, lapchl12, lap_arg12, lap_pan16)

lapnew$cyear <- paste(lapnew$cname, lapnew$year, sep = "_") 

lapnew <- lapnew %>%
  relocate (cyear, .before = pais)



#JUNTAR TUDO:

## dplyr::renameS E MAIS SELECT PARA DEIXAR SÓ O QUE IMPORTA DE congruência (E ALGUMAS OUTRAS DE IDEOLOGIA PARA ANÁLISES DESCRITIVAS):

names (lapnew) <- gsub("ros", "ROES", names(lapnew)) 

names (lapnew) <- gsub("w14", "abortion", names(lapnew)) 


# INCLUIR/RENOMEAR VARIÁVEIS ESPECÍFICAS DE PAÍSES 

# Variável "votou" - quando tem a opção de Legislativo usar ela:
#VB15 - El salvador 2012 tem, e é preciso usar porque é um ano bem diferente da Presidencial 

#CHILE e NICARAGUA TEM LEG mas só uma pergunta "foi votar", porque eleição Leg é junto com Pres em ambos casos. Tudo ok. 
#Outros países não parecem ter variável "votou Legislativo", pode ser um problema se a eleição for em dias diferentes!




# PERU ESTAVA NA SELEÇÃO INICIAL MAS ACABOU FICANDO FORA DO EMD NA VERSÃO ANTERIOR - MOTIVO: FALTOU VARIÁVEL "VOTOU":

# PERU 2010 SEM VB2 (tem a outra relacionada a segundo turno!). Ver se precisa dele ou se algum dos outros cobre para eleição
#daquela Legislatura - 2008 cobriria 2006 a 2011 (PROBLEMA É QUE TEM MENOS ISSUES), 2012 cobriria 2011-2016


#ANTES DISSO MELHOR JÁ RESOLVER TODOS MISSINGS!


#CRIANDO NOVA VAR: 

lap <- lap %>% mutate (
  voted = case_when(
    cname == "SLV" ~ (vb15),
    TRUE          ~ vb2
  )
)

### DEPOIS DISSO: 


# PERU PRECISA DA VOTO 2º! Em qual? Talvez melhor deixar um dos dois bancos de lado!


#JUNTAR, E PARTIR PARA OS EMD. QUANDO FOR COM DDPLY DEVE FUNCIONAR SE ESTIVER TUDO CERTO NO PELA
#QUANDO FOR SEM, PRECISA ESCREVER O CÓDIGO (COM COPY+PASTE) PARA OS BANCOS NOVOS






##### CODEBOOKS #####


##### GRAND MERGE - VOTO #####

#TODAS COM "vb" que pareciam interessar no momento:

# vb2
# "Voted in Last Presidential Election" 
# vb2v 
# "Voted in the Second Round of the Presidential Elections" 
# vb3_04 
# "Vote in 2002 Presidential election" 
# vb3_06 
# "Por quién votó para Presidente en las últimas elecciones presidenciales" 
# vb3_08 
# "Por quién votó para Presidente en las últimas elecciones presidenciales" 
# vb3_10 
# "Por quién votó para Presidente en las últimas elecciones presidenciales" 
# vb3_12 
# "Por quién votó para Presidente en las últimas elecciones presidenciales" 
# vb3n2_18 
# "Vote Choice in the Second Round of Presidential Elections" 
# vb3n_14 
# "Presidential Vote Choice" 
# vb3n_16 
# "Presidential Vote Choice" 
# vb3n_18 
# "Presidential Vote Choice" 
# 
# vb20 
# "Vote Intention in Next Presidential Election" 
# 
# vb5 
# "VB5. Quando votou, qual foi a razão mais importante de seu voto?" 
# vb6 
# "Voted for Deputy in the Last Election" 
# vb8 
# "Cuando votó, ¿Cuál fue la razón más importante de su voto?" 

##### 2008 #####

# ROS1. The (Country) government, instead of the private sector, should own the most important
# enterprises and industries of the country. How much do you agree or disagree with this statement?

# ROS2. The (Country) government, more than individuals, is the most responsible for ensuring the
# well-being of the people. To what extent do you agree or disagree with this statement?
   
# ROS3. The (Country) government, more than the private sector, has the primarily responsible for
# creating jobs. To what extent to do you agree or disagree with this statement?
   
# ROS4. The (Country) government should implement firm policies to reduce income inequality
# between the rich and the poor. To what extent do you agree or disagree with this statement?
  


# Q10 = income
# Q10. Into which of the following income ranges does the total monthly income of this household fit,
# including remittances from abroad and the income of all the working adults and children?
#   [10 deciles based on the currency and distribution of the country]
# (00) No income
# (01) Less than $25
# (02) $26- $50
# (03) $51-$100
# (04) $101-$150
# (05) $151-$200
# (06) $201-$300
# (07) $301-$400
# (08) $401-500
# (09) $501-$750
# (10) More than $751
# (88) DK/DR 
# 
# 
# ETID. Do you consider yourself white, mestizo, indigenous, Afro-country (black), mulatto, or of another
# race?
#   (1) White (2) Mestizo (3) Indigenous (4) Black o Afro-country (5) Mulatto
# (7) Other (8) DK/DR
# 
# 
# L1 = left-right ( 1 left 10 right, dk==88)
# 
# IMMIG1. To what degree do you agree that the (country) government provides social services such as
# healthcare, education, housing, to foreigners who come to live or work in this country?
#   1= Str agree, 5 str desagree 8 DK
# 
# 
# 
# EPP1. Thinking of political parties in general, to what extent do [country's] political parties
# represent their voters well? 
# 
# 
#   
# 
# ##### 2006 #####
# 
# PR6 -  Em um país como o Brasil, é obrigação do governo diminuir as diferenças entre os muito ricos e os muito pobres
# 
# 
# VB12B. Nas eleições para deputado federal, há um número muito grande de candidatos. Como
# o sr/sra decide em quem votar para esse tipo de cargo? [Espontânea]
# 
# 
# BRAVB12. Em quem o(a) senhor(a) votou para deputado federal? 
#   
#   
# WVS 2006 - aborto! 

  
  
  
