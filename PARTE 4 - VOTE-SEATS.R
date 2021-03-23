### PARA AUTO-SUFICI?NCIA SEM OS SCRIPTS ANTERIORES
library(xlsx)
library(dplyr)
library(emdist)
library(haven)
library(readxl)
library(xtable)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

elavotes <- read_xlsx("ela_voteclean.xlsx") # ESSE DATASET ESTÁ COM Nº DE LINHAS UM POUCO DIFERENTES
#PARA O "ELA_PUTVOTES", ACHO QUE EM PAÍSES COMO CHILE QUE ESTÁ A DIFERENÇA. Não sei se é algum erro
#ou algum procedimento que fiz quando inseri manualmente dados de share of votes, share of seats. 

names (elavotes) <- gsub("ROES10", "ROES", names(elavotes)) 

elavotes <- elavotes %>% dplyr::rename (same_sex = VAL1, ideol_self = ID1)

#### PARA FICAR TOTALMENTE AUTO-SUFICIENTE PRECISO TRAZER TODOS OS SUBSETS POR PA?S PARA C? TAMB?M:

BRA_elavotes <- elavotes %>% filter (cname == "BRA")
BOL_elavotes <- elavotes %>% filter (cname == "BOL")
CHL_elavotes <- elavotes %>% filter (cname == "CHL")
COL_elavotes <- elavotes %>% filter (cname == "COL")
CRI_elavotes <- elavotes %>% filter (cname == "CRI")
DOM_elavotes <- elavotes %>% filter (cname == "DOM")
GTM_elavotes <- elavotes %>% filter (cname == "GTM")
HND_elavotes <- elavotes %>% filter (cname == "HND")
NIC_elavotes <- elavotes %>% filter (cname == "NIC")
PER_elavotes <- elavotes %>% filter (cname == "URY")
URY_elavotes <- elavotes %>% filter (cname == "URY")



##### TAMB?M PARA ESSE SCRIPT SER AUTO-SUFICIENTE (CRIA??O DOS SAMPLES):

###### SAMPLES PARA VOTE SHARES : SHARES / SEATS * 1000 #####

BRA_elavotes$sample <- (BRA_elavotes$votes/ BRA_elavotes$interview) * 1000 
BRA_smp <- BRA_elavotes[rep(seq(nrow(BRA_elavotes)), BRA_elavotes$sample),]
BRA_smp$pop <- as.factor("y")


BOL_elavotes$sample <- (BOL_elavotes$votes/ BOL_elavotes$interview) * 1000 
BOL_smp <- BOL_elavotes[rep(seq(nrow(BOL_elavotes)), BOL_elavotes$sample),]
BOL_smp$pop <- as.factor("y")

CHL_elavotes$sample <- (CHL_elavotes$votes/ CHL_elavotes$interview) * 1000 
CHL_smp <- CHL_elavotes[rep(seq(nrow(CHL_elavotes)), CHL_elavotes$sample),]
CHL_smp$pop <- as.factor("y")

COL_elavotes$sample <- (COL_elavotes$votes/ COL_elavotes$interview) * 1000 
COL_smp <- COL_elavotes[rep(seq(nrow(COL_elavotes)), COL_elavotes$sample),]
COL_smp$pop <- as.factor("y")

CRI_elavotes$sample <- (CRI_elavotes$votes/ CRI_elavotes$interview) * 1000 
CRI_smp <- CRI_elavotes[rep(seq(nrow(CRI_elavotes)), CRI_elavotes$sample),]
CRI_smp$pop <- as.factor("y")

DOM_elavotes$sample <- (DOM_elavotes$votes/ DOM_elavotes$interview) * 1000 
DOM_smp <- DOM_elavotes[rep(seq(nrow(DOM_elavotes)), DOM_elavotes$sample),]
DOM_smp$pop <- as.factor("y")

GTM_elavotes$sample <- (GTM_elavotes$votes/ GTM_elavotes$interview) * 1000 
GTM_smp <- GTM_elavotes[rep(seq(nrow(GTM_elavotes)), GTM_elavotes$sample),]
GTM_smp$pop <- as.factor("y")

HND_elavotes$sample <- (HND_elavotes$votes/ HND_elavotes$interview) * 1000 
HND_smp <- HND_elavotes[rep(seq(nrow(HND_elavotes)), HND_elavotes$sample),]
HND_smp$pop <- as.factor("y")

NIC_elavotes$sample <- (NIC_elavotes$votes/ NIC_elavotes$interview) * 1000 
NIC_smp <- NIC_elavotes[rep(seq(nrow(NIC_elavotes)), NIC_elavotes$sample),]
NIC_smp$pop <- as.factor("y")

PER_elavotes$sample <- (PER_elavotes$votes/ PER_elavotes$interview) * 1000 
PER_smp <- PER_elavotes[rep(seq(nrow(PER_elavotes)), PER_elavotes$sample),]
PER_smp$pop <- as.factor("y")

URY_elavotes$sample <- (URY_elavotes$votes/ URY_elavotes$interview) * 1000 
URY_smp <- URY_elavotes[rep(seq(nrow(URY_elavotes)), URY_elavotes$sample),]
URY_smp$pop <- as.factor("y")



##### SAMPLES PARA SEATS ##### 

BRA_elavotes$smpseats <- (BRA_elavotes$seats/ BRA_elavotes$interview) * 10

BRA_seats <- BRA_elavotes[rep(seq(nrow(BRA_elavotes)), BRA_elavotes$smpseats),]
BRA_seats$pop <- as.factor("x")

BOL_elavotes$smpseats <- (BOL_elavotes$seats/ BOL_elavotes$interview) * 10 
BOL_seats <- BOL_elavotes[rep(seq(nrow(BOL_elavotes)), BOL_elavotes$smpseats),]
BOL_seats$pop <- as.factor("x")

CHL_elavotes$smpseats <- (CHL_elavotes$seats/ CHL_elavotes$interview) * 10 
CHL_seats <- CHL_elavotes[rep(seq(nrow(CHL_elavotes)), CHL_elavotes$smpseats),]
CHL_seats$pop <- as.factor("x")

COL_elavotes$smpseats <- (COL_elavotes$seats/ COL_elavotes$interview) * 10 
COL_seats <- COL_elavotes[rep(seq(nrow(COL_elavotes)), COL_elavotes$smpseats),]
COL_seats$pop <- as.factor("x")

CRI_elavotes$smpseats <- (CRI_elavotes$seats/ CRI_elavotes$interview) * 10 
CRI_seats <- CRI_elavotes[rep(seq(nrow(CRI_elavotes)), CRI_elavotes$smpseats),]
CRI_seats$pop <- as.factor("x")

DOM_elavotes$smpseats <- (DOM_elavotes$seats/ DOM_elavotes$interview) * 10 
DOM_seats <- DOM_elavotes[rep(seq(nrow(DOM_elavotes)), DOM_elavotes$smpseats),]
DOM_seats$pop <- as.factor("x")

GTM_elavotes$smpseats <- (GTM_elavotes$seats/ GTM_elavotes$interview) * 10 
GTM_seats <- GTM_elavotes[rep(seq(nrow(GTM_elavotes)), GTM_elavotes$smpseats),]
GTM_seats$pop <- as.factor("x")

HND_elavotes$smpseats <- (HND_elavotes$seats/ HND_elavotes$interview) * 10 
HND_seats <- HND_elavotes[rep(seq(nrow(HND_elavotes)), HND_elavotes$smpseats),]
HND_seats$pop <- as.factor("x")

NIC_elavotes$smpseats <- (NIC_elavotes$seats/ NIC_elavotes$interview) * 10 
NIC_seats <- NIC_elavotes[rep(seq(nrow(NIC_elavotes)), NIC_elavotes$smpseats),]
NIC_seats$pop <- as.factor("x")

PER_elavotes$smpseats <- (PER_elavotes$seats/ PER_elavotes$interview) * 10 
PER_seats <- PER_elavotes[rep(seq(nrow(PER_elavotes)), PER_elavotes$smpseats),]
PER_seats$pop <- as.factor("x")

URY_elavotes$smpseats <- (URY_elavotes$seats/ URY_elavotes$interview) * 10 
URY_seats <- URY_elavotes[rep(seq(nrow(URY_elavotes)), URY_elavotes$smpseats),]
URY_seats$pop <- as.factor("x")




##### ROES 1 ######

###### SELECTING VARIABLES #####


# VOTES SAMPLE: 
BRA_smp_cong <- BRA_smp %>% select (ROES1, pop) 
BOL_smp_cong <- BOL_smp %>% select (ROES1, pop) 
CHL_smp_cong <- CHL_smp %>% select (ROES1, pop) 
COL_smp_cong <- COL_smp %>% select (ROES1, pop) 
CRI_smp_cong <- CRI_smp %>% select (ROES1, pop) 
DOM_smp_cong <- DOM_smp %>% select (ROES1, pop) 
GTM_smp_cong <- GTM_smp %>% select (ROES1, pop) 
HND_smp_cong <- HND_smp %>% select (ROES1, pop) 
NIC_smp_cong <- NIC_smp %>% select (ROES1, pop) 
PER_smp_cong <- PER_smp %>% select (ROES1, pop) 
URY_smp_cong <- URY_smp %>% select (ROES1, pop) 


# SEATS : 
BRA_seats_roes <- BRA_seats %>% select (ROES1, pop) 
BOL_seats_roes <- BOL_seats %>% select (ROES1, pop) 
CHL_seats_roes <- CHL_seats %>% select (ROES1, pop) 
COL_seats_roes <- COL_seats %>% select (ROES1, pop) 
CRI_seats_roes <- CRI_seats %>% select (ROES1, pop) 
DOM_seats_roes <- DOM_seats %>% select (ROES1, pop) 
GTM_seats_roes <- GTM_seats %>% select (ROES1, pop) 
HND_seats_roes <- HND_seats %>% select (ROES1, pop) 
NIC_seats_roes <- NIC_seats %>% select (ROES1, pop)
PER_seats_roes <- PER_seats %>% select (ROES1, pop) 
URY_seats_roes <- URY_seats %>% select (ROES1, pop) 


###### JUNTANDO OS BANCOS #####

BRA_voteseats <- rbind (BRA_smp_cong, BRA_seats_roes)
BOL_voteseats <- rbind (BOL_smp_cong, BOL_seats_roes)
CHL_voteseats <- rbind (CHL_smp_cong, CHL_seats_roes)
COL_voteseats <- rbind (COL_smp_cong, COL_seats_roes)
CRI_voteseats <- rbind (CRI_smp_cong, CRI_seats_roes)
DOM_voteseats <- rbind (DOM_smp_cong, DOM_seats_roes)
GTM_voteseats <- rbind (GTM_smp_cong, GTM_seats_roes)
HND_voteseats <- rbind (HND_smp_cong, HND_seats_roes)
NIC_voteseats <- rbind (NIC_smp_cong, NIC_seats_roes)
PER_voteseats <- rbind (PER_smp_cong, PER_seats_roes)
URY_voteseats <- rbind (URY_smp_cong, URY_seats_roes)


## dplyr::rename PARA SAMPS: 
BRA_voteseats <- BRA_voteseats %>% dplyr::rename (samps = ROES1) 
BOL_voteseats <- BOL_voteseats %>% dplyr::rename (samps = ROES1) 
CHL_voteseats <- CHL_voteseats %>% dplyr::rename (samps = ROES1) 
COL_voteseats <- COL_voteseats %>% dplyr::rename (samps = ROES1) 
CRI_voteseats <- CRI_voteseats %>% dplyr::rename (samps = ROES1) 
DOM_voteseats <- DOM_voteseats %>% dplyr::rename (samps = ROES1) 
GTM_voteseats <- GTM_voteseats %>% dplyr::rename (samps = ROES1) 
HND_voteseats <- HND_voteseats %>% dplyr::rename (samps = ROES1) 
NIC_voteseats <- NIC_voteseats %>% dplyr::rename (samps = ROES1) 
PER_voteseats <- PER_voteseats %>% dplyr::rename (samps = ROES1) 
URY_voteseats <- URY_voteseats %>% dplyr::rename (samps = ROES1) 

## As.numeric (alguma coisa nas idas e vindas para planilha, csv, desconfigurou as vari?veis para "character"):

BRA_voteseats$samps <- as.numeric(BRA_voteseats$samps)
BOL_voteseats$samps <- as.numeric(BOL_voteseats$samps)
CHL_voteseats$samps <- as.numeric(CHL_voteseats$samps)
COL_voteseats$samps <- as.numeric(COL_voteseats$samps)
CRI_voteseats$samps <- as.numeric(CRI_voteseats$samps)
DOM_voteseats$samps <- as.numeric(DOM_voteseats$samps)
GTM_voteseats$samps <- as.numeric(GTM_voteseats$samps)
HND_voteseats$samps <- as.numeric(HND_voteseats$samps)
NIC_voteseats$samps <- as.numeric(NIC_voteseats$samps)
PER_voteseats$samps <- as.numeric(PER_voteseats$samps)
URY_voteseats$samps <- as.numeric(URY_voteseats$samps)


### TIRANDO MISSING:

BRA_voteseats <- completeFun (BRA_voteseats, "samps")
BOL_voteseats <- completeFun (BOL_voteseats, "samps")
CHL_voteseats <- completeFun (CHL_voteseats, "samps")
COL_voteseats <- completeFun (COL_voteseats, "samps")
CRI_voteseats <- completeFun (CRI_voteseats, "samps")
DOM_voteseats <- completeFun (DOM_voteseats, "samps")
GTM_voteseats <- completeFun (GTM_voteseats, "samps")
HND_voteseats <- completeFun (HND_voteseats, "samps")
NIC_voteseats <- completeFun (NIC_voteseats, "samps")
PER_voteseats <- completeFun (PER_voteseats, "samps")
URY_voteseats <- completeFun (URY_voteseats, "samps")

##### CONGRU?NCIA

### EMD:

BRA_emd_seats_roes1<- emd.dis(BRA_voteseats)
BOL_emd_seats_roes1<- emd.dis(BOL_voteseats)
CHL_emd_seats_roes1<- emd.dis(CHL_voteseats)
COL_emd_seats_roes1<- emd.dis(COL_voteseats)
CRI_emd_seats_roes1<- emd.dis(CRI_voteseats)
DOM_emd_seats_roes1<- emd.dis(DOM_voteseats)
GTM_emd_seats_roes1<- emd.dis(GTM_voteseats)
HND_emd_seats_roes1<- emd.dis(HND_voteseats)
NIC_emd_seats_roes1<- emd.dis(NIC_voteseats)
PER_emd_seats_roes1<- emd.dis(PER_voteseats)
URY_emd_seats_roes1<- emd.dis(URY_voteseats)

#CDF Overlap:
BRA_cdf_seats_roes1<- cdf.overlap(BRA_voteseats)
BOL_cdf_seats_roes1<- cdf.overlap(BOL_voteseats)
CHL_cdf_seats_roes1<- cdf.overlap(CHL_voteseats)
COL_cdf_seats_roes1<- cdf.overlap(COL_voteseats)
CRI_cdf_seats_roes1<- cdf.overlap(CRI_voteseats)
DOM_cdf_seats_roes1<- cdf.overlap(DOM_voteseats)
GTM_cdf_seats_roes1<- cdf.overlap(GTM_voteseats)
HND_cdf_seats_roes1<- cdf.overlap(HND_voteseats)
NIC_cdf_seats_roes1<- cdf.overlap(NIC_voteseats)
PER_cdf_seats_roes1<- cdf.overlap(PER_voteseats)
URY_cdf_seats_roes1<- cdf.overlap(URY_voteseats)

## Difference in means:
BRA_mean_seats_roes1<- dmeans (BRA_voteseats)
BOL_mean_seats_roes1<- dmeans (BOL_voteseats)
CHL_mean_seats_roes1<- dmeans (CHL_voteseats)
COL_mean_seats_roes1<- dmeans (COL_voteseats)
CRI_mean_seats_roes1<- dmeans (CRI_voteseats)
DOM_mean_seats_roes1<- dmeans (DOM_voteseats)
GTM_mean_seats_roes1<- dmeans (GTM_voteseats)
HND_mean_seats_roes1<- dmeans (HND_voteseats)
NIC_mean_seats_roes1<- dmeans (NIC_voteseats)
PER_mean_seats_roes1<- dmeans (PER_voteseats)
URY_mean_seats_roes1<- dmeans (URY_voteseats)

### PLANILHAS: 

# CDF_seats_ROES1 <- rbind (BRA_cdf_seats_roes1, BOL_cdf_seats_roes1, CHL_cdf_seats_roes1,
#                           COL_cdf_seats_roes1, CRI_cdf_seats_roes1, DOM_cdf_seats_roes1,
#                           GTM_cdf_seats_roes1, HND_cdf_seats_roes1, NIC_cdf_seats_roes1,
#                           PER_cdf_seats_roes1)

EMD_seats_ROES1 <- rbind (BRA_emd_seats_roes1, BOL_emd_seats_roes1, CHL_emd_seats_roes1,
                          COL_emd_seats_roes1, CRI_emd_seats_roes1, DOM_emd_seats_roes1,
                          GTM_emd_seats_roes1, HND_emd_seats_roes1, NIC_emd_seats_roes1,
                          PER_emd_seats_roes1, URY_emd_seats_roes1)


mean_seats_ROES1 <- rbind (BRA_mean_seats_roes1, BOL_mean_seats_roes1, CHL_mean_seats_roes1,
                           COL_mean_seats_roes1, CRI_mean_seats_roes1, DOM_mean_seats_roes1,
                           GTM_mean_seats_roes1, HND_mean_seats_roes1, NIC_mean_seats_roes1,
                           PER_mean_seats_roes1, URY_mean_seats_roes1)

###### SAME-SEX MARRIAGE ##### - S? SUBSTITUIR TUDO DE ROES POR SMSEX E OS NOMES 



###### SAME_SEX MARRIAGE ###################################################

BRA_smp_same_sex <- BRA_smp %>% select (same_sex, pop) 
BOL_smp_same_sex <- BOL_smp %>% select (same_sex, pop) 
CHL_smp_same_sex <- CHL_smp %>% select (same_sex, pop) 
COL_smp_same_sex <- COL_smp %>% select (same_sex, pop) 
CRI_smp_same_sex <- CRI_smp %>% select (same_sex, pop) 
DOM_smp_same_sex <- DOM_smp %>% select (same_sex, pop) 
GTM_smp_same_sex <- GTM_smp %>% select (same_sex, pop) 
HND_smp_same_sex <- HND_smp %>% select (same_sex, pop) 
NIC_smp_same_sex <- NIC_smp %>% select (same_sex, pop) 
PER_smp_same_sex <- PER_smp %>% select (same_sex, pop) 
URY_smp_same_sex <- URY_smp %>% select (same_sex, pop) 


# SEATS : 
BRA_seats_same_sex <- BRA_seats %>% select (same_sex, pop) 
BOL_seats_same_sex <- BOL_seats %>% select (same_sex, pop) 
CHL_seats_same_sex <- CHL_seats %>% select (same_sex, pop) 
COL_seats_same_sex <- COL_seats %>% select (same_sex, pop) 
CRI_seats_same_sex <- CRI_seats %>% select (same_sex, pop) 
DOM_seats_same_sex <- DOM_seats %>% select (same_sex, pop) 
GTM_seats_same_sex <- GTM_seats %>% select (same_sex, pop) 
HND_seats_same_sex <- HND_seats %>% select (same_sex, pop) 
NIC_seats_same_sex <- NIC_seats %>% select (same_sex, pop) 
PER_seats_same_sex <- PER_seats %>% select (same_sex, pop) 
URY_seats_same_sex <- URY_seats %>% select (same_sex, pop) 


###### JUNTANDO OS BANCOS #####

BRA_voteseats <- rbind (BRA_smp_same_sex, BRA_seats_same_sex)
BOL_voteseats <- rbind (BOL_smp_same_sex, BOL_seats_same_sex)
CHL_voteseats <- rbind (CHL_smp_same_sex, CHL_seats_same_sex)
COL_voteseats <- rbind (COL_smp_same_sex, COL_seats_same_sex)
CRI_voteseats <- rbind (CRI_smp_same_sex, CRI_seats_same_sex)
DOM_voteseats <- rbind (DOM_smp_same_sex, DOM_seats_same_sex)
GTM_voteseats <- rbind (GTM_smp_same_sex, GTM_seats_same_sex)
HND_voteseats <- rbind (HND_smp_same_sex, HND_seats_same_sex)
NIC_voteseats <- rbind (NIC_smp_same_sex, NIC_seats_same_sex)
PER_voteseats <- rbind (PER_smp_same_sex, PER_seats_same_sex)
URY_voteseats <- rbind (URY_smp_same_sex, URY_seats_same_sex)


## dplyr::rename PARA SAMPS: 
BRA_voteseats <- BRA_voteseats %>% dplyr::rename (samps = same_sex) 
BOL_voteseats <- BOL_voteseats %>% dplyr::rename (samps = same_sex) 
CHL_voteseats <- CHL_voteseats %>% dplyr::rename (samps = same_sex) 
COL_voteseats <- COL_voteseats %>% dplyr::rename (samps = same_sex) 
CRI_voteseats <- CRI_voteseats %>% dplyr::rename (samps = same_sex) 
DOM_voteseats <- DOM_voteseats %>% dplyr::rename (samps = same_sex) 
GTM_voteseats <- GTM_voteseats %>% dplyr::rename (samps = same_sex) 
HND_voteseats <- HND_voteseats %>% dplyr::rename (samps = same_sex) 
NIC_voteseats <- NIC_voteseats %>% dplyr::rename (samps = same_sex) 
PER_voteseats <- PER_voteseats %>% dplyr::rename (samps = same_sex) 
URY_voteseats <- URY_voteseats %>% dplyr::rename (samps = same_sex) 

## As.numeric (alguma coisa nas idas e vindas para planilha, csv, desconfigurou as vari?veis para "character"):

BRA_voteseats$samps <- as.numeric(BRA_voteseats$samps)
BOL_voteseats$samps <- as.numeric(BOL_voteseats$samps)
CHL_voteseats$samps <- as.numeric(CHL_voteseats$samps)
COL_voteseats$samps <- as.numeric(COL_voteseats$samps)
CRI_voteseats$samps <- as.numeric(CRI_voteseats$samps)
DOM_voteseats$samps <- as.numeric(DOM_voteseats$samps)
GTM_voteseats$samps <- as.numeric(GTM_voteseats$samps)
HND_voteseats$samps <- as.numeric(HND_voteseats$samps)
NIC_voteseats$samps <- as.numeric(NIC_voteseats$samps)
PER_voteseats$samps <- as.numeric(PER_voteseats$samps)
URY_voteseats$samps <- as.numeric(URY_voteseats$samps)


### TIRANDO MISSING:

BRA_voteseats <- completeFun (BRA_voteseats, "samps")
BOL_voteseats <- completeFun (BOL_voteseats, "samps")
CHL_voteseats <- completeFun (CHL_voteseats, "samps")
COL_voteseats <- completeFun (COL_voteseats, "samps")
CRI_voteseats <- completeFun (CRI_voteseats, "samps")
DOM_voteseats <- completeFun (DOM_voteseats, "samps")
GTM_voteseats <- completeFun (GTM_voteseats, "samps")
HND_voteseats <- completeFun (HND_voteseats, "samps")
NIC_voteseats <- completeFun (NIC_voteseats, "samps")
PER_voteseats <- completeFun (PER_voteseats, "samps")
URY_voteseats <- completeFun (URY_voteseats, "samps")

##### CONGRU?NCIA

### EMD:

BRA_emd_seats_same_sex<- emd.dis(BRA_voteseats)
BOL_emd_seats_same_sex<- emd.dis(BOL_voteseats)
CHL_emd_seats_same_sex<- emd.dis(CHL_voteseats)
COL_emd_seats_same_sex<- emd.dis(COL_voteseats)
CRI_emd_seats_same_sex<- emd.dis(CRI_voteseats)
DOM_emd_seats_same_sex<- emd.dis(DOM_voteseats)
GTM_emd_seats_same_sex<- emd.dis(GTM_voteseats)
HND_emd_seats_same_sex<- emd.dis(HND_voteseats)
NIC_emd_seats_same_sex<- emd.dis(NIC_voteseats)
PER_emd_seats_same_sex<- emd.dis(PER_voteseats)
URY_emd_seats_same_sex<- emd.dis(URY_voteseats)

#CDF Overlap:
BRA_cdf_seats_same_sex<- cdf.overlap(BRA_voteseats)
BOL_cdf_seats_same_sex<- cdf.overlap(BOL_voteseats)
CHL_cdf_seats_same_sex<- cdf.overlap(CHL_voteseats)
COL_cdf_seats_same_sex<- cdf.overlap(COL_voteseats)
CRI_cdf_seats_same_sex<- cdf.overlap(CRI_voteseats)
DOM_cdf_seats_same_sex<- cdf.overlap(DOM_voteseats)
GTM_cdf_seats_same_sex<- cdf.overlap(GTM_voteseats)
HND_cdf_seats_same_sex<- cdf.overlap(HND_voteseats)
NIC_cdf_seats_same_sex<- cdf.overlap(NIC_voteseats)
PER_cdf_seats_same_sex<- cdf.overlap(PER_voteseats)
URY_cdf_seats_same_sex<- cdf.overlap(URY_voteseats)

## Difference in means:
BRA_mean_seats_same_sex<- dmeans (BRA_voteseats)
BOL_mean_seats_same_sex<- dmeans (BOL_voteseats)
CHL_mean_seats_same_sex<- dmeans (CHL_voteseats)
COL_mean_seats_same_sex<- dmeans (COL_voteseats)
CRI_mean_seats_same_sex<- dmeans (CRI_voteseats)
DOM_mean_seats_same_sex<- dmeans (DOM_voteseats)
GTM_mean_seats_same_sex<- dmeans (GTM_voteseats)
HND_mean_seats_same_sex<- dmeans (HND_voteseats)
NIC_mean_seats_same_sex<- dmeans (NIC_voteseats)
PER_mean_seats_same_sex<- dmeans (PER_voteseats)
URY_mean_seats_same_sex<- dmeans (URY_voteseats)

### PLANILHAS: 

# CDF_seats_same_sex <- rbind (BRA_cdf_seats_same_sex, BOL_cdf_seats_same_sex, CHL_cdf_seats_same_sex,
#                              COL_cdf_seats_same_sex, CRI_cdf_seats_same_sex, DOM_cdf_seats_same_sex,
#                              GTM_cdf_seats_same_sex, HND_cdf_seats_same_sex, NIC_cdf_seats_same_sex,
#                              PER_cdf_seats_same_sex)

EMD_seats_same_sex <- rbind (BRA_emd_seats_same_sex, BOL_emd_seats_same_sex, CHL_emd_seats_same_sex,
                             COL_emd_seats_same_sex, CRI_emd_seats_same_sex, DOM_emd_seats_same_sex,
                             GTM_emd_seats_same_sex, HND_emd_seats_same_sex, NIC_emd_seats_same_sex,
                             PER_emd_seats_same_sex, URY_emd_seats_same_sex)


write.csv (list(BRA_mean_seats_same_sex, BOL_mean_seats_same_sex, CHL_mean_seats_same_sex,
                COL_mean_seats_same_sex, CRI_mean_seats_same_sex, DOM_mean_seats_same_sex,
                GTM_mean_seats_same_sex, HND_mean_seats_same_sex, NIC_mean_seats_same_sex,
                PER_mean_seats_same_sex, URY_mean_seats_same_sex), "Seats_samesex.csv")



###### LEFT-RIGHT SELF PLACEMENT #########################################

BRA_smp_LR <- BRA_smp %>% select (ideol_self, pop) 
BOL_smp_LR <- BOL_smp %>% select (ideol_self, pop) 
CHL_smp_LR <- CHL_smp %>% select (ideol_self, pop) 
COL_smp_LR <- COL_smp %>% select (ideol_self, pop) 
CRI_smp_LR <- CRI_smp %>% select (ideol_self, pop) 
DOM_smp_LR <- DOM_smp %>% select (ideol_self, pop) 
GTM_smp_LR <- GTM_smp %>% select (ideol_self, pop) 
HND_smp_LR <- HND_smp %>% select (ideol_self, pop) 
NIC_smp_LR <- NIC_smp %>% select (ideol_self, pop) 
PER_smp_LR <- PER_smp %>% select (ideol_self, pop) 
URY_smp_LR <- URY_smp %>% select (ideol_self, pop) 


# SEATS : 
BRA_seats_LR <- BRA_seats %>% select (ideol_self, pop) 
BOL_seats_LR <- BOL_seats %>% select (ideol_self, pop) 
CHL_seats_LR <- CHL_seats %>% select (ideol_self, pop) 
COL_seats_LR <- COL_seats %>% select (ideol_self, pop) 
CRI_seats_LR <- CRI_seats %>% select (ideol_self, pop) 
DOM_seats_LR <- DOM_seats %>% select (ideol_self, pop) 
GTM_seats_LR <- GTM_seats %>% select (ideol_self, pop) 
HND_seats_LR <- HND_seats %>% select (ideol_self, pop) 
NIC_seats_LR <- NIC_seats %>% select (ideol_self, pop) 
PER_seats_LR <- PER_seats %>% select (ideol_self, pop) 
URY_seats_LR <- URY_seats %>% select (ideol_self, pop) 


###### 1.JUN??O E EDI??O DOS BANCOS #####

BRA_voteseats_LR <- rbind (BRA_smp_LR, BRA_seats_LR)
BOL_voteseats_LR <- rbind (BOL_smp_LR, BOL_seats_LR)
CHL_voteseats_LR <- rbind (CHL_smp_LR, CHL_seats_LR)
COL_voteseats_LR <- rbind (COL_smp_LR, COL_seats_LR)
CRI_voteseats_LR <- rbind (CRI_smp_LR, CRI_seats_LR)
DOM_voteseats_LR <- rbind (DOM_smp_LR, DOM_seats_LR)
GTM_voteseats_LR <- rbind (GTM_smp_LR, GTM_seats_LR)
HND_voteseats_LR <- rbind (HND_smp_LR, HND_seats_LR)
NIC_voteseats_LR <- rbind (NIC_smp_LR, NIC_seats_LR)
PER_voteseats_LR <- rbind (PER_smp_LR, PER_seats_LR)
URY_voteseats_LR <- rbind (URY_smp_LR, URY_seats_LR)


## dplyr::rename PARA SAMPS: 
BRA_voteseats_LR <- BRA_voteseats_LR %>% dplyr::rename (samps = ideol_self) 
BOL_voteseats_LR <- BOL_voteseats_LR %>% dplyr::rename (samps = ideol_self) 
CHL_voteseats_LR <- CHL_voteseats_LR %>% dplyr::rename (samps = ideol_self) 
COL_voteseats_LR <- COL_voteseats_LR %>% dplyr::rename (samps = ideol_self) 
CRI_voteseats_LR <- CRI_voteseats_LR %>% dplyr::rename (samps = ideol_self) 
DOM_voteseats_LR <- DOM_voteseats_LR %>% dplyr::rename (samps = ideol_self) 
GTM_voteseats_LR <- GTM_voteseats_LR %>% dplyr::rename (samps = ideol_self) 
HND_voteseats_LR <- HND_voteseats_LR %>% dplyr::rename (samps = ideol_self) 
NIC_voteseats_LR <- NIC_voteseats_LR %>% dplyr::rename (samps = ideol_self) 
PER_voteseats_LR <- PER_voteseats_LR %>% dplyr::rename (samps = ideol_self) 
URY_voteseats_LR <- URY_voteseats_LR %>% dplyr::rename (samps = ideol_self) 

## As.numeric (alguma coisa nas idas e vindas para planilha, csv, desconfigurou as vari?veis para "character"):

BRA_voteseats_LR$samps <- as.numeric(BRA_voteseats_LR$samps)
BOL_voteseats_LR$samps <- as.numeric(BOL_voteseats_LR$samps)
CHL_voteseats_LR$samps <- as.numeric(CHL_voteseats_LR$samps)
COL_voteseats_LR$samps <- as.numeric(COL_voteseats_LR$samps)
CRI_voteseats_LR$samps <- as.numeric(CRI_voteseats_LR$samps)
DOM_voteseats_LR$samps <- as.numeric(DOM_voteseats_LR$samps)
GTM_voteseats_LR$samps <- as.numeric(GTM_voteseats_LR$samps)
HND_voteseats_LR$samps <- as.numeric(HND_voteseats_LR$samps)
NIC_voteseats_LR$samps <- as.numeric(NIC_voteseats_LR$samps)
PER_voteseats_LR$samps <- as.numeric(PER_voteseats_LR$samps)
URY_voteseats_LR$samps <- as.numeric(URY_voteseats_LR$samps)


### TIRANDO MISSING:

BRA_voteseats_LR <- completeFun (BRA_voteseats_LR, "samps")
BOL_voteseats_LR <- completeFun (BOL_voteseats_LR, "samps")
CHL_voteseats_LR <- completeFun (CHL_voteseats_LR, "samps")
COL_voteseats_LR <- completeFun (COL_voteseats_LR, "samps")
CRI_voteseats_LR <- completeFun (CRI_voteseats_LR, "samps")
DOM_voteseats_LR <- completeFun (DOM_voteseats_LR, "samps")
GTM_voteseats_LR <- completeFun (GTM_voteseats_LR, "samps")
HND_voteseats_LR <- completeFun (HND_voteseats_LR, "samps")
NIC_voteseats_LR <- completeFun (NIC_voteseats_LR, "samps")
PER_voteseats_LR <- completeFun (PER_voteseats_LR, "samps")
URY_voteseats_LR <- completeFun (URY_voteseats_LR, "samps")

##### 2. CONGRU?NCIA #####

### EMD:

BRA_emd_seats_LR<- emd.dis(BRA_voteseats_LR)
BOL_emd_seats_LR<- emd.dis(BOL_voteseats_LR)
CHL_emd_seats_LR<- emd.dis(CHL_voteseats_LR)
COL_emd_seats_LR<- emd.dis(COL_voteseats_LR)
CRI_emd_seats_LR<- emd.dis(CRI_voteseats_LR)
DOM_emd_seats_LR<- emd.dis(DOM_voteseats_LR)
GTM_emd_seats_LR<- emd.dis(GTM_voteseats_LR)
HND_emd_seats_LR<- emd.dis(HND_voteseats_LR)
NIC_emd_seats_LR<- emd.dis(NIC_voteseats_LR)
PER_emd_seats_LR<- emd.dis(PER_voteseats_LR)
URY_emd_seats_LR<- emd.dis(URY_voteseats_LR)

#CDF Overlap:
BRA_cdf_seats_LR<- cdf.overlap(BRA_voteseats_LR)
BOL_cdf_seats_LR<- cdf.overlap(BOL_voteseats_LR)
CHL_cdf_seats_LR<- cdf.overlap(CHL_voteseats_LR)
COL_cdf_seats_LR<- cdf.overlap(COL_voteseats_LR)
CRI_cdf_seats_LR<- cdf.overlap(CRI_voteseats_LR)
DOM_cdf_seats_LR<- cdf.overlap(DOM_voteseats_LR)
GTM_cdf_seats_LR<- cdf.overlap(GTM_voteseats_LR)
HND_cdf_seats_LR<- cdf.overlap(HND_voteseats_LR)
NIC_cdf_seats_LR<- cdf.overlap(NIC_voteseats_LR)
PER_cdf_seats_LR<- cdf.overlap(PER_voteseats_LR)
URY_cdf_seats_LR<- cdf.overlap(URY_voteseats_LR)

## Difference in means:
BRA_mean_seats_LR<- dmeans (BRA_voteseats_LR)
BOL_mean_seats_LR<- dmeans (BOL_voteseats_LR)
CHL_mean_seats_LR<- dmeans (CHL_voteseats_LR)
COL_mean_seats_LR<- dmeans (COL_voteseats_LR)
CRI_mean_seats_LR<- dmeans (CRI_voteseats_LR)
DOM_mean_seats_LR<- dmeans (DOM_voteseats_LR)
GTM_mean_seats_LR<- dmeans (GTM_voteseats_LR)
HND_mean_seats_LR<- dmeans (HND_voteseats_LR)
NIC_mean_seats_LR<- dmeans (NIC_voteseats_LR)
PER_mean_seats_LR<- dmeans (PER_voteseats_LR)
URY_mean_seats_LR<- dmeans (URY_voteseats_LR)

##### 3. TABELAS DE RESULTADOS #####

EMD_seats_LR <- rbind (BRA_emd_seats_LR, BOL_emd_seats_LR, CHL_emd_seats_LR,
                       COL_emd_seats_LR, CRI_emd_seats_LR, DOM_emd_seats_LR,
                       GTM_emd_seats_LR, HND_emd_seats_LR, NIC_emd_seats_LR,
                       PER_emd_seats_LR, URY_emd_seats_LR)

# 
# tab <- cbind(c(BOL_mean_seats_LR,	BRA_mean_seats_LR, CHL_mean_seats_LR,
#                COL_mean_seats_LR, CRI_mean_seats_LR, DOM_mean_seats_LR,
#                GTM_mean_seats_LR, HND_mean_seats_LR, NIC_mean_seats_LR,	        
#                PER_mean_seats_LR), 
#              c(BOL_emd_seats_LR, BRA_emd_seats_LR, CHL_emd_seats_LR,
#                COL_emd_seats_LR, CRI_emd_seats_LR, DOM_emd_seats_LR,
#                GTM_emd_seats_LR, HND_emd_seats_LR, NIC_emd_seats_LR,	        
#                PER_emd_seats_LR),
#              c(BOL_cdf_seats_LR,	BRA_cdf_seats_LR, CHL_cdf_seats_LR,
#                COL_cdf_seats_LR, CRI_cdf_seats_LR, DOM_cdf_seats_LR,
#                GTM_cdf_seats_LR, HND_cdf_seats_LR, NIC_cdf_seats_LR,	        
#                PER_cdf_seats_LR)) 


# dimnames(tab) <- list(c("BOLIVIA","BRAZIL", "CHILE", "COLOMBIA", "COSTA RICA", "REP DOM", "GUATEMALA", 
#                         "HONDURAS", "NICARAGUA", "URUGUAY"),
#                       c("Diff in Means","EMD", "CDF overlap"))

# stargazer(tab, title = "Votes-to-seats Congruence - LEFT - RIGHT")




##### ROES 2 ######

###### SELECTING VARIABLES #####


# VOTES SAMPLE: 
BRA_smp_cong <- BRA_smp %>% select (ROES2, pop) 
BOL_smp_cong <- BOL_smp %>% select (ROES2, pop) 
CHL_smp_cong <- CHL_smp %>% select (ROES2, pop) 
COL_smp_cong <- COL_smp %>% select (ROES2, pop) 
CRI_smp_cong <- CRI_smp %>% select (ROES2, pop) 
DOM_smp_cong <- DOM_smp %>% select (ROES2, pop) 
GTM_smp_cong <- GTM_smp %>% select (ROES2, pop) 
HND_smp_cong <- HND_smp %>% select (ROES2, pop) 
NIC_smp_cong <- NIC_smp %>% select (ROES2, pop) 
PER_smp_cong <- PER_smp %>% select (ROES2, pop) 
URY_smp_cong <- URY_smp %>% select (ROES2, pop) 


# SEATS : 
BRA_seats_roes2 <- BRA_seats %>% select (ROES2, pop) 
BOL_seats_roes2 <- BOL_seats %>% select (ROES2, pop) 
CHL_seats_roes2 <- CHL_seats %>% select (ROES2, pop) 
COL_seats_roes2 <- COL_seats %>% select (ROES2, pop) 
CRI_seats_roes2 <- CRI_seats %>% select (ROES2, pop) 
DOM_seats_roes2 <- DOM_seats %>% select (ROES2, pop) 
GTM_seats_roes2 <- GTM_seats %>% select (ROES2, pop) 
HND_seats_roes2 <- HND_seats %>% select (ROES2, pop) 
NIC_seats_roes2 <- NIC_seats %>% select (ROES2, pop)
PER_seats_roes2 <- PER_seats %>% select (ROES2, pop) 
URY_seats_roes2 <- URY_seats %>% select (ROES2, pop) 


###### JUNTANDO OS BANCOS #####

BRA_voteseats <- rbind (BRA_smp_cong, BRA_seats_roes2)
BOL_voteseats <- rbind (BOL_smp_cong, BOL_seats_roes2)
CHL_voteseats <- rbind (CHL_smp_cong, CHL_seats_roes2)
COL_voteseats <- rbind (COL_smp_cong, COL_seats_roes2)
CRI_voteseats <- rbind (CRI_smp_cong, CRI_seats_roes2)
DOM_voteseats <- rbind (DOM_smp_cong, DOM_seats_roes2)
GTM_voteseats <- rbind (GTM_smp_cong, GTM_seats_roes2)
HND_voteseats <- rbind (HND_smp_cong, HND_seats_roes2)
NIC_voteseats <- rbind (NIC_smp_cong, NIC_seats_roes2)
PER_voteseats <- rbind (PER_smp_cong, PER_seats_roes2)
URY_voteseats <- rbind (URY_smp_cong, URY_seats_roes2)


## dplyr::rename PARA SAMPS: 
BRA_voteseats <- BRA_voteseats %>% dplyr::rename (samps = ROES2) 
BOL_voteseats <- BOL_voteseats %>% dplyr::rename (samps = ROES2) 
CHL_voteseats <- CHL_voteseats %>% dplyr::rename (samps = ROES2) 
COL_voteseats <- COL_voteseats %>% dplyr::rename (samps = ROES2) 
CRI_voteseats <- CRI_voteseats %>% dplyr::rename (samps = ROES2) 
DOM_voteseats <- DOM_voteseats %>% dplyr::rename (samps = ROES2) 
GTM_voteseats <- GTM_voteseats %>% dplyr::rename (samps = ROES2) 
HND_voteseats <- HND_voteseats %>% dplyr::rename (samps = ROES2) 
NIC_voteseats <- NIC_voteseats %>% dplyr::rename (samps = ROES2) 
PER_voteseats <- PER_voteseats %>% dplyr::rename (samps = ROES2) 
URY_voteseats <- URY_voteseats %>% dplyr::rename (samps = ROES2) 

## As.numeric (alguma coisa nas idas e vindas para planilha, csv, desconfigurou as vari?veis para "character"):

BRA_voteseats$samps <- as.numeric(BRA_voteseats$samps)
BOL_voteseats$samps <- as.numeric(BOL_voteseats$samps)
CHL_voteseats$samps <- as.numeric(CHL_voteseats$samps)
COL_voteseats$samps <- as.numeric(COL_voteseats$samps)
CRI_voteseats$samps <- as.numeric(CRI_voteseats$samps)
DOM_voteseats$samps <- as.numeric(DOM_voteseats$samps)
GTM_voteseats$samps <- as.numeric(GTM_voteseats$samps)
HND_voteseats$samps <- as.numeric(HND_voteseats$samps)
NIC_voteseats$samps <- as.numeric(NIC_voteseats$samps)
PER_voteseats$samps <- as.numeric(PER_voteseats$samps)
URY_voteseats$samps <- as.numeric(URY_voteseats$samps)


### TIRANDO MISSING:

BRA_voteseats <- completeFun (BRA_voteseats, "samps")
BOL_voteseats <- completeFun (BOL_voteseats, "samps")
CHL_voteseats <- completeFun (CHL_voteseats, "samps")
COL_voteseats <- completeFun (COL_voteseats, "samps")
CRI_voteseats <- completeFun (CRI_voteseats, "samps")
DOM_voteseats <- completeFun (DOM_voteseats, "samps")
GTM_voteseats <- completeFun (GTM_voteseats, "samps")
HND_voteseats <- completeFun (HND_voteseats, "samps")
NIC_voteseats <- completeFun (NIC_voteseats, "samps")
PER_voteseats <- completeFun (PER_voteseats, "samps")
URY_voteseats <- completeFun (URY_voteseats, "samps")

##### CONGRU?NCIA

### EMD:

BRA_emd_seats_roes2<- emd.dis(BRA_voteseats)
BOL_emd_seats_roes2<- emd.dis(BOL_voteseats)
CHL_emd_seats_roes2<- emd.dis(CHL_voteseats)
COL_emd_seats_roes2<- emd.dis(COL_voteseats)
CRI_emd_seats_roes2<- emd.dis(CRI_voteseats)
DOM_emd_seats_roes2<- emd.dis(DOM_voteseats)
GTM_emd_seats_roes2<- emd.dis(GTM_voteseats)
HND_emd_seats_roes2<- emd.dis(HND_voteseats)
NIC_emd_seats_roes2<- emd.dis(NIC_voteseats)
PER_emd_seats_roes2<- emd.dis(PER_voteseats)
URY_emd_seats_roes2<- emd.dis(URY_voteseats)

#CDF Overlap:
BRA_cdf_seats_roes2<- cdf.overlap(BRA_voteseats)
BOL_cdf_seats_roes2<- cdf.overlap(BOL_voteseats)
CHL_cdf_seats_roes2<- cdf.overlap(CHL_voteseats)
COL_cdf_seats_roes2<- cdf.overlap(COL_voteseats)
CRI_cdf_seats_roes2<- cdf.overlap(CRI_voteseats)
DOM_cdf_seats_roes2<- cdf.overlap(DOM_voteseats)
GTM_cdf_seats_roes2<- cdf.overlap(GTM_voteseats)
HND_cdf_seats_roes2<- cdf.overlap(HND_voteseats)
NIC_cdf_seats_roes2<- cdf.overlap(NIC_voteseats)
PER_cdf_seats_roes2<- cdf.overlap(PER_voteseats)
URY_cdf_seats_roes2<- cdf.overlap(URY_voteseats)

## Difference in means:
BRA_mean_seats_roes2<- dmeans (BRA_voteseats)
BOL_mean_seats_roes2<- dmeans (BOL_voteseats)
CHL_mean_seats_roes2<- dmeans (CHL_voteseats)
COL_mean_seats_roes2<- dmeans (COL_voteseats)
CRI_mean_seats_roes2<- dmeans (CRI_voteseats)
DOM_mean_seats_roes2<- dmeans (DOM_voteseats)
GTM_mean_seats_roes2<- dmeans (GTM_voteseats)
HND_mean_seats_roes2<- dmeans (HND_voteseats)
NIC_mean_seats_roes2<- dmeans (NIC_voteseats)
PER_mean_seats_roes2<- dmeans (PER_voteseats)
URY_mean_seats_roes2<- dmeans (URY_voteseats)

### PLANILHAS: 

# CDF_seats_ROES2 <- rbind (BRA_cdf_seats_roes2, BOL_cdf_seats_roes2, CHL_cdf_seats_roes2,
#                           COL_cdf_seats_roes2, CRI_cdf_seats_roes2, DOM_cdf_seats_roes2,
#                           GTM_cdf_seats_roes2, HND_cdf_seats_roes2, NIC_cdf_seats_roes2,
#                           PER_cdf_seats_roes2)

EMD_seats_ROES2 <- rbind (BRA_emd_seats_roes2, BOL_emd_seats_roes2, CHL_emd_seats_roes2,
                          COL_emd_seats_roes2, CRI_emd_seats_roes2, DOM_emd_seats_roes2,
                          GTM_emd_seats_roes2, HND_emd_seats_roes2, NIC_emd_seats_roes2,
                          PER_emd_seats_roes2, URY_emd_seats_roes2)


mean_seats_ROES2 <- rbind (BRA_mean_seats_roes2, BOL_mean_seats_roes2, CHL_mean_seats_roes2,
                           COL_mean_seats_roes2, CRI_mean_seats_roes2, DOM_mean_seats_roes2,
                           GTM_mean_seats_roes2, HND_mean_seats_roes2, NIC_mean_seats_roes2,
                           PER_mean_seats_roes2, URY_mean_seats_roes2)


##### ROES 3 ######

###### SELECTING VARIABLES #####


# VOTES SAMPLE: 
BRA_smp_cong <- BRA_smp %>% select (ROES3, pop) 
BOL_smp_cong <- BOL_smp %>% select (ROES3, pop) 
CHL_smp_cong <- CHL_smp %>% select (ROES3, pop) 
COL_smp_cong <- COL_smp %>% select (ROES3, pop) 
CRI_smp_cong <- CRI_smp %>% select (ROES3, pop) 
DOM_smp_cong <- DOM_smp %>% select (ROES3, pop) 
GTM_smp_cong <- GTM_smp %>% select (ROES3, pop) 
HND_smp_cong <- HND_smp %>% select (ROES3, pop) 
NIC_smp_cong <- NIC_smp %>% select (ROES3, pop) 
PER_smp_cong <- PER_smp %>% select (ROES3, pop) 
URY_smp_cong <- URY_smp %>% select (ROES3, pop) 


# SEATS : 
BRA_seats_roes3 <- BRA_seats %>% select (ROES3, pop) 
BOL_seats_roes3 <- BOL_seats %>% select (ROES3, pop) 
CHL_seats_roes3 <- CHL_seats %>% select (ROES3, pop) 
COL_seats_roes3 <- COL_seats %>% select (ROES3, pop) 
CRI_seats_roes3 <- CRI_seats %>% select (ROES3, pop) 
DOM_seats_roes3 <- DOM_seats %>% select (ROES3, pop) 
GTM_seats_roes3 <- GTM_seats %>% select (ROES3, pop) 
HND_seats_roes3 <- HND_seats %>% select (ROES3, pop) 
NIC_seats_roes3 <- NIC_seats %>% select (ROES3, pop)
PER_seats_roes3 <- PER_seats %>% select (ROES3, pop) 
URY_seats_roes3 <- URY_seats %>% select (ROES3, pop) 


###### JUNTANDO OS BANCOS #####

BRA_voteseats <- rbind (BRA_smp_cong, BRA_seats_roes3)
BOL_voteseats <- rbind (BOL_smp_cong, BOL_seats_roes3)
CHL_voteseats <- rbind (CHL_smp_cong, CHL_seats_roes3)
COL_voteseats <- rbind (COL_smp_cong, COL_seats_roes3)
CRI_voteseats <- rbind (CRI_smp_cong, CRI_seats_roes3)
DOM_voteseats <- rbind (DOM_smp_cong, DOM_seats_roes3)
GTM_voteseats <- rbind (GTM_smp_cong, GTM_seats_roes3)
HND_voteseats <- rbind (HND_smp_cong, HND_seats_roes3)
NIC_voteseats <- rbind (NIC_smp_cong, NIC_seats_roes3)
PER_voteseats <- rbind (PER_smp_cong, PER_seats_roes3)
URY_voteseats <- rbind (URY_smp_cong, URY_seats_roes3)


## dplyr::rename PARA SAMPS: 
BRA_voteseats <- BRA_voteseats %>% dplyr::rename (samps = ROES3) 
BOL_voteseats <- BOL_voteseats %>% dplyr::rename (samps = ROES3) 
CHL_voteseats <- CHL_voteseats %>% dplyr::rename (samps = ROES3) 
COL_voteseats <- COL_voteseats %>% dplyr::rename (samps = ROES3) 
CRI_voteseats <- CRI_voteseats %>% dplyr::rename (samps = ROES3) 
DOM_voteseats <- DOM_voteseats %>% dplyr::rename (samps = ROES3) 
GTM_voteseats <- GTM_voteseats %>% dplyr::rename (samps = ROES3) 
HND_voteseats <- HND_voteseats %>% dplyr::rename (samps = ROES3) 
NIC_voteseats <- NIC_voteseats %>% dplyr::rename (samps = ROES3) 
PER_voteseats <- PER_voteseats %>% dplyr::rename (samps = ROES3) 
URY_voteseats <- URY_voteseats %>% dplyr::rename (samps = ROES3) 

## As.numeric (alguma coisa nas idas e vindas para planilha, csv, desconfigurou as vari?veis para "character"):

BRA_voteseats$samps <- as.numeric(BRA_voteseats$samps)
BOL_voteseats$samps <- as.numeric(BOL_voteseats$samps)
CHL_voteseats$samps <- as.numeric(CHL_voteseats$samps)
COL_voteseats$samps <- as.numeric(COL_voteseats$samps)
CRI_voteseats$samps <- as.numeric(CRI_voteseats$samps)
DOM_voteseats$samps <- as.numeric(DOM_voteseats$samps)
GTM_voteseats$samps <- as.numeric(GTM_voteseats$samps)
HND_voteseats$samps <- as.numeric(HND_voteseats$samps)
NIC_voteseats$samps <- as.numeric(NIC_voteseats$samps)
PER_voteseats$samps <- as.numeric(PER_voteseats$samps)
URY_voteseats$samps <- as.numeric(URY_voteseats$samps)


### TIRANDO MISSING:

BRA_voteseats <- completeFun (BRA_voteseats, "samps")
BOL_voteseats <- completeFun (BOL_voteseats, "samps")
CHL_voteseats <- completeFun (CHL_voteseats, "samps")
COL_voteseats <- completeFun (COL_voteseats, "samps")
CRI_voteseats <- completeFun (CRI_voteseats, "samps")
DOM_voteseats <- completeFun (DOM_voteseats, "samps")
GTM_voteseats <- completeFun (GTM_voteseats, "samps")
HND_voteseats <- completeFun (HND_voteseats, "samps")
NIC_voteseats <- completeFun (NIC_voteseats, "samps")
PER_voteseats <- completeFun (PER_voteseats, "samps")
URY_voteseats <- completeFun (URY_voteseats, "samps")

##### CONGRU?NCIA

### EMD:

BRA_emd_seats_roes3<- emd.dis(BRA_voteseats)
BOL_emd_seats_roes3<- emd.dis(BOL_voteseats)
CHL_emd_seats_roes3<- emd.dis(CHL_voteseats)
COL_emd_seats_roes3<- emd.dis(COL_voteseats)
CRI_emd_seats_roes3<- emd.dis(CRI_voteseats)
DOM_emd_seats_roes3<- emd.dis(DOM_voteseats)
GTM_emd_seats_roes3<- emd.dis(GTM_voteseats)
HND_emd_seats_roes3<- emd.dis(HND_voteseats)
NIC_emd_seats_roes3<- emd.dis(NIC_voteseats)
PER_emd_seats_roes3<- emd.dis(PER_voteseats)
URY_emd_seats_roes3<- emd.dis(URY_voteseats)

#CDF Overlap:
BRA_cdf_seats_roes3<- cdf.overlap(BRA_voteseats)
BOL_cdf_seats_roes3<- cdf.overlap(BOL_voteseats)
CHL_cdf_seats_roes3<- cdf.overlap(CHL_voteseats)
COL_cdf_seats_roes3<- cdf.overlap(COL_voteseats)
CRI_cdf_seats_roes3<- cdf.overlap(CRI_voteseats)
DOM_cdf_seats_roes3<- cdf.overlap(DOM_voteseats)
GTM_cdf_seats_roes3<- cdf.overlap(GTM_voteseats)
HND_cdf_seats_roes3<- cdf.overlap(HND_voteseats)
NIC_cdf_seats_roes3<- cdf.overlap(NIC_voteseats)
PER_cdf_seats_roes3<- cdf.overlap(PER_voteseats)
URY_cdf_seats_roes3<- cdf.overlap(URY_voteseats)

## Difference in means:
BRA_mean_seats_roes3<- dmeans (BRA_voteseats)
BOL_mean_seats_roes3<- dmeans (BOL_voteseats)
CHL_mean_seats_roes3<- dmeans (CHL_voteseats)
COL_mean_seats_roes3<- dmeans (COL_voteseats)
CRI_mean_seats_roes3<- dmeans (CRI_voteseats)
DOM_mean_seats_roes3<- dmeans (DOM_voteseats)
GTM_mean_seats_roes3<- dmeans (GTM_voteseats)
HND_mean_seats_roes3<- dmeans (HND_voteseats)
NIC_mean_seats_roes3<- dmeans (NIC_voteseats)
PER_mean_seats_roes3<- dmeans (PER_voteseats)
URY_mean_seats_roes3<- dmeans (URY_voteseats)

### PLANILHAS: 

# CDF_seats_ROES3 <- rbind (BRA_cdf_seats_roes3, BOL_cdf_seats_roes3, CHL_cdf_seats_roes3,
#                           COL_cdf_seats_roes3, CRI_cdf_seats_roes3, DOM_cdf_seats_roes3,
#                           GTM_cdf_seats_roes3, HND_cdf_seats_roes3, NIC_cdf_seats_roes3,
#                           PER_cdf_seats_roes3)

EMD_seats_ROES3 <- rbind (BRA_emd_seats_roes3, BOL_emd_seats_roes3, CHL_emd_seats_roes3,
                          COL_emd_seats_roes3, CRI_emd_seats_roes3, DOM_emd_seats_roes3,
                          GTM_emd_seats_roes3, HND_emd_seats_roes3, NIC_emd_seats_roes3,
                          PER_emd_seats_roes3, URY_emd_seats_roes3)


mean_seats_ROES3 <- rbind (BRA_mean_seats_roes3, BOL_mean_seats_roes3, CHL_mean_seats_roes3,
                           COL_mean_seats_roes3, CRI_mean_seats_roes3, DOM_mean_seats_roes3,
                           GTM_mean_seats_roes3, HND_mean_seats_roes3, NIC_mean_seats_roes3,
                           PER_mean_seats_roes3, URY_mean_seats_roes3)


rownames (EMD_seats_ROES1) <- gsub("_emd_seats_roes1", "", rownames (EMD_seats_ROES1))
rownames (EMD_seats_ROES2) <- gsub("_emd_seats_roes2", "", rownames (EMD_seats_ROES2))
rownames (EMD_seats_ROES3) <- gsub("_emd_seats_roes3", "", rownames (EMD_seats_ROES3))
rownames (EMD_seats_LR) <- gsub("_emd_seats_LR", "", rownames (EMD_seats_LR))
rownames (EMD_seats_same_sex) <- gsub("_emd_seats_same_sex", "", rownames (EMD_seats_same_sex))

EMD_seats <- bind_cols(EMD_seats_ROES1, EMD_seats_ROES2,  EMD_seats_ROES3, EMD_seats_same_sex,
                       EMD_seats_LR)

# Ficou sem nome cada coluna, de resto tudo certo
# Tamb?m est? invertido Brasil com Bol?via (em todos outros est? Bol?via antes, ordem alfab?tica!)

write.xlsx(EMD_seats, "Congruence - VOTES TO SEATS.xlsx")

