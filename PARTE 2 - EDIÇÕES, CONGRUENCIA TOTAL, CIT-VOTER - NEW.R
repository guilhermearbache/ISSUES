#rm(list=ls()[!(ls() %in% c("ela"))]) INCLUIR LAPOP

library(ggplot2); library(sfsmisc); library(emdist)
library(foreign); library(plyr); library(sandwich) 
library(lmtest)

library(tidyverse)

library(haven)
library(descr)

######## PELA ########

load("C:/Users/livia/OneDrive - usp.br/TESE/LATAM/LASPP/elanew.RData")

###

### TRANSFORMAR ROES PARA FICAR PARECIDO COM LAPOP.

names (ela) <- gsub("ROES10", "ROES", names(ela)) 


### AGORA VAMOS APRONTAR OS BANCOS PARA A FUNção DE LUPU 

#CRIANDO A COLUNA IDENTIFICADORA DOS DOIS DATA.FRAMES PARA TEREM DISTRIBUIção COMPARADA NA MEDIDA DE congruência:
ela$pop <- as.factor("x")
lapfinal$pop <- as.factor("y")



#### teste - caminho 1:

elacong <- ela %>%
  dplyr::rename (same_sex = VAL1, ideol_self = ID1) %>%
  select (cname, pop, same_sex, ideol_self, ROES1:ROES6) 

lapcong <- lapfinal %>%
  select (cname, pop, same_sex, ideol_self, ROES1:ROES6) 



### OBS: ACABEI DEIXANDO ABORTION DE LADO, ATÉ POR não SER A VARIÁVEL IGUAL MESMO (QUERIA USAR W14a PELO MENOS, 
#PARA UMA COMPARAÇÃO DESCRITIVA, MAS VAMOS DEIXAR ISSO PARA DEPOIS PORQUE FICA MAIS COMPLICADO O RBIND). SE
## QUISER UTILIZAR ESSA OU QUALQUER OUTRA VARIÁVEL PARA COMPARAR, INSERIR NO CÓDIGO ACIMA.

#JUNTANDO: 

lapela <- rbind(elacong, lapcong)


save(lapela,file = "lapela.RData")


# na.omit - vou criar uma função para fazer o na.omit de cada variável específica, aí não preciso ficar criando um
#banco só com as variáveis a usar para cada medida com a função de Lupu:

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}



### Agora, procedemos para a congruência em si. 


#-----------------FUNÇÕES - LUPU & WARNER------------------------------------

# Difference in means
dmeans <- function(data){
  df <- data
  x <- as.matrix(df$samps[which(df$pop == "x")])
  y <- as.matrix(df$samps[which(df$pop == "y")])
  abs(mean(x,na.rm=T)-mean(y,na.rm=T))
}
# CDF overlap
cdf.overlap <- function(data, q = 512) { 
  # no default length for sequences, but we'll use density's 512
  df <- data
  x <- as.matrix(na.omit(df$samps[which(df$pop == "x")]))
  y <- as.matrix(na.omit(df$samps[which(df$pop == "y")]))
  Fx <- ecdf(x); Fy <- ecdf(y)
  lower <- min(x,y) 
  upper <- max(x,y)
  z <- seq(lower,upper,((upper-lower)/q))
  Fxz <- Fx(z); Fyz <- Fy(z) 
  sum(abs(Fxz-Fyz))
}
# PDF overlap
pdf.overlap <- function(data, q = 512){ # q = 512 is the default for density()
  df <- data
  fx <- as.vector(na.omit(df$samps[which(df$pop == "x")]))
  fy <- as.vector(na.omit(df$samps[which(df$pop == "y")]))
  lower <- min(c(fx, fy))
  upper <- max(c(fx, fy))
  dx <- density(fx, from=lower, to=upper, n = q)
  dy <- density(fy, from=lower, to=upper, n = q)
  d <- data.frame(location=dx$x, x.den=dx$y, y.den=dy$y)
  d$intersect <- pmin(d$x.den, d$y.den)
  integrate.xy(d$location, d$intersect)
}
# EMD
emd.dis <- function(data, metric = "manhattan", iterations=100000){
  df <- data
  x <- as.matrix(df$samps[which(df$pop == "x")])
  y <- as.matrix(df$samps[which(df$pop == "y")])
  weight.x <- rep(1/nrow(x),nrow(x))
  weight.y <- rep(1/nrow(y),nrow(y))
  emdw(x,weight.x,y,weight.y,dist=metric,max.iter=iterations)
}

#-------------TOTAL CONGRUENCE-------------------


##### SAME_SEX MARRIAGE ##### 

#Renomear a variável de interesse para samps:

df_smsex <- lapela

names(df_smsex)[names(df_smsex) == 'same_sex'] <- 'samps'

df_smsex <- completeFun(df_smsex, "samps")


#EMD
emd_smsex <- ddply(df_smsex, .(cname), emd.dis)
 

#OUTRAS MEDIDAS:
cdf_smsex <- ddply(df_smsex, .(cname), cdf.overlap)
pdf_smsex <- ddply(df_smsex, .(cname), pdf.overlap)
means_smsex <- ddply(df_smsex, .(cname), dmeans)

###### ROES1 #######

df_ros1 <- lapela

names(df_ros1)[names(df_ros1) == 'ROES1'] <- 'samps'

df_ros1 <- completeFun(df_ros1, "samps")

#EMD
emd_ros1 <- ddply(df_ros1, .(cname), emd.dis)


#OUTRAS MEDIDAS:
cdf_ros1 <- ddply(df_ros1, .(cname), cdf.overlap)
pdf_ros1 <- ddply(df_ros1, .(cname), pdf.overlap)
means_ros1 <- ddply(df_ros1, .(cname), dmeans)


##### ROES2 #####

df_ros2 <- lapela

names(df_ros2)[names(df_ros2) == 'ROES2'] <- 'samps'

df_ros2 <- completeFun(df_ros2, "samps")

#EMD
emd_ros2 <- ddply(df_ros2, .(cname), emd.dis)


#OUTRAS MEDIDAS:
cdf_ros2 <- ddply(df_ros2, .(cname), cdf.overlap)
pdf_ros2 <- ddply(df_ros2, .(cname), pdf.overlap)
means_ros2 <- ddply(df_ros2, .(cname), dmeans)


## ROS3 
###### ROES 3##########

df_ros3 <- lapela

names(df_ros3)[names(df_ros3) == 'ROES3'] <- 'samps'

df_ros3 <- completeFun(df_ros3, "samps")

#EMD
emd_ros3 <- ddply(df_ros3, .(cname), emd.dis)


#OUTRAS MEDIDAS:
cdf_ros3 <- ddply(df_ros3, .(cname), cdf.overlap)
pdf_ros3 <- ddply(df_ros3, .(cname), pdf.overlap)
means_ros3 <- ddply(df_ros3, .(cname), dmeans)


##### LEFT-RIGHT #####

#Renomear a variável de interesse para samps:

df_ideol_self <- lapela

names(df_ideol_self)[names(df_ideol_self) == 'ideol_self'] <- 'samps'

df_ideol_self <- completeFun(df_ideol_self, "samps")


#EMD
emd_ideol_self <- ddply(df_ideol_self, .(cname), emd.dis)



means_ideol_self <- ddply(df_ideol_self, .(cname), dmeans)



###### PLANILHAS DE RESULTADOS ###################

library("xlsx")

write.xlsx(list(emd_smsex=emd_smsex, cdf_smsex = cdf_smsex, pdf_smsex = pdf_smsex, congm = means_smsex), "Totcong_sm.xlsx")

write.xlsx(list(emd_ros1=emd_ros1, cdf_ros1 = cdf_ros1, pdf_ros1 = pdf_ros1, congm = means_ros1), "Totcong_ros1.xlsx")

write.xlsx(list(emd_ros2=emd_ros2, cdf_ros2 = cdf_ros2, pdf_ros2 = pdf_ros2, congm = means_ros2), "Totcong_ros2.xlsx")

write.xlsx(list(emd_ros3=emd_ros3, cdf_ros3 = cdf_ros3, pdf_ros3 = pdf_ros3, congm = means_ros3), "Totcong_ros3.xlsx")

write.xlsx(list(emd_ideol_self=emd_ideol_self,  congm = means_ideol_self), "Totcong_ideol_self.xlsx")


##### ETAPAS DA congruência #####

###### Citizen-voter #####


### FIRST REMOVE MISSING: 

lapcitizen <- completeFun(lapfinal, "vb2")

#Fazer subset de eleitores:

lapvoter <- lapcitizen %>% 
  filter (vb2==1)

#x e y para identificar subset e set
lapcitizen$pop <- as.factor("x")
lapvoter$pop <- as.factor("y")

#rbind subset + set

lap_citvot <- rbind(lapcitizen, lapvoter)

#rodar tudo  

##### SAME-SEX MARRIAGE #####:

df_citvot_smsex <- lap_citvot

names(df_citvot_smsex)[names(df_citvot_smsex) == 'same_sex'] <- 'samps'

df_citvot_smsex <- completeFun(df_citvot_smsex, "samps")

#EMD
emd_cv_smsex <- ddply(df_citvot_smsex, .(cname), emd.dis)


##### ROES 1#### 

df_citvot_ros1 <- lap_citvot

names(df_citvot_ros1)[names(df_citvot_ros1) == 'ROES1'] <- 'samps'

df_citvot_ros1 <- completeFun(df_citvot_ros1, "samps")

#EMD
emd_cv_ros1 <- ddply(df_citvot_ros1, .(cname), emd.dis)

##### ROES2#### 

df_citvot_ros2 <- lap_citvot

names(df_citvot_ros2)[names(df_citvot_ros2) == 'ROES2'] <- 'samps'

df_citvot_ros2 <- completeFun(df_citvot_ros2, "samps")

#EMD
emd_cv_ros2 <- ddply(df_citvot_ros2, .(cname), emd.dis)

### ROES 3 ###: 
df_citvot_ros3 <- lap_citvot

names(df_citvot_ros3)[names(df_citvot_ros3) == 'ROES3'] <- 'samps'

df_citvot_ros3 <- completeFun(df_citvot_ros3, "samps")

#EMD
emd_cv_ros3 <- ddply(df_citvot_ros3, .(cname), emd.dis)

## LEFT-RIGHT:

df_citvot_LR <- lap_citvot

names(df_citvot_LR)[names(df_citvot_LR) == 'ideol_self'] <- 'samps'

df_citvot_LR <- completeFun(df_citvot_LR, "samps")

#EMD
emd_cv_LR <- ddply(df_citvot_LR, .(cname), emd.dis)

write.xlsx(list(emd_cv_smsex=emd_cv_smsex, emd_cv_ros1=emd_cv_ros1, emd_cv_ros2=emd_cv_ros2, emd_cv_ros3=emd_cv_ros3, 
                emd_cv_LR = emd_cv_LR), "Cit_voter_congruence.xlsx")


