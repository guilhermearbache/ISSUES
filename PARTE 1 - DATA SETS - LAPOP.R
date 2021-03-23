library(haven)
library(tidyverse)
library(dplyr)

#BANCO DE DADOS DE OPINIÃO PÚBLICA - BARÔMETRO DAS AMÉRICAS (LAPOP): 
#Esse banco já tem uma versão compilada de vários anos/países, em arquivo .dta. Vamos carregá-la:
#lap <- read_dta ("C:\\Users\\user\\Google Drive\\BANCOS DE DADOS\\LAPOP\\AmericasBarometer Grand Merge 2004-2014 v3.0_FREE.dta")

### VOU FAZER ALTERAÇÕES POR PARTES PORQUE ESSE BANCO É MUITO PESADO, EVITAR BAIXAR ELE INTEIRO É SEMPRE BOM:


#lapop <- lap  %>% filter(pais < 22 & pais != 15)

#OBS: Esta pesquisa tem países de todos os países do continente (daí o nome "Barômetro das Américas),
#por isso selecionamos apenas as observações correspondentes aos países latino-americanos contemplados
#na pesquisa com parlamentares. Para nossa sorte todos eles estavam agrupados até o valor "22" para países.

#JÁ DEIXO SALVO PARA USAR ESSA VERSÃO POR ENQUANTO: 

#save(lapop, file = "C:/Users/user/Google Drive/TESE - Congruência/LAPOP_1012.RData")


#### posso sempre começar daqui :


lapop <- read_dta("Lapop_merge_2004-2018.dta") 

lapop <- lapop  %>% filter(pais < 22 & year<2014 & year > 2007)



# AGORA, SELECIONO AS VARIÁVEIS DE INTERESSE


library(sjlabelled)

lapop <- remove_all_labels(lapop)

lapop <- lapop %>%
  select (pais, year, q1, q2, q3c, q10, ed, l1, l1b, a4, 
          starts_with("ros"), d6, starts_with("gi"), starts_with("w14"), vb1:vb2, vb6) %>%
  mutate(cname = recode(pais, `1` = "MEX", `2` = "GTM", `3` = "SLV", `4` = "HND", `5` = "NIC", `6` ="CRI",      # CRIANDO CNAME 
                        `7` = "PAN", `8` = "COL", `9` ="ECU", `10` = "BOL", `11` = "PER", `12`= "PRY", 
                        `13` = "CHL", `14` = "URY", `15` = "BRA", `16` = "VEN", `17` = "ARG", `21` = "DOM")) 

lapop$cyear <- paste(lapop$cname, lapop$year, sep = "_") 

lapop <- lapop %>%
  relocate (cyear, .before = pais)


# SÓ PERU 2010 SEM VB2 (tem a outra relacionada a segundo turno!). Ver se precisa dele ou se algum dos outros cobre para eleição
#daquela Legislatura - 2008 cobriria 2006 a 2011 (PROBLEMA É QUE TEM MENOS ISSUES), 2012 cobriria 2011-2016


#### ROS 1 E ROS 2 TEM EM TODOS!




##### 2008 #####

# ROS1. The (Country) government, instead of the private sector, should own the most important
# enterprises and industries of the country. How much do you agree or disagree with this statement?
#   ROS1
# ROS2. The (Country) government, more than individuals, is the most responsible for ensuring the
# well-being of the people. To what extent do you agree or disagree with this statement?
#   ROS2
# ROS3. The (Country) government, more than the private sector, has the primarily responsible for
# creating jobs. To what extent to do you agree or disagree with this statement?
#   ROS3
# ROS4. The (Country) government should implement firm policies to reduce income inequality
# between the rich and the poor. To what extent do you agree or disagree with this statement?
#   ROS4


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

  
  
  
