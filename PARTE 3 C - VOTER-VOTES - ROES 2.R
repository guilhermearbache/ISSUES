library(readxl)

##### CRIANDO FUNÇÃO PARA TIRAR NA #####
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

### BAIXANDO OS BANCOS E FAZENDO ALGUMAS ALTERAÇÕES INICIAIS

load ("lapfinal.RData")

elavotes <- read_xlsx("ela_voteclean.xlsx")

names (elavotes) <- gsub("ROES10", "ROES", names(elavotes)) 

lapcitizen <- completeFun(lapfinal, c("vb2", "ROES2"))

elavotes <- completeFun(elavotes, "ROES2")

#Subset de eleitores:

lapvoter <- lapcitizen %>% 
  filter (vb2==1)


### Só consegui fazer a análise em separado de todos países, poderia fazer tudo isso 
# em conjunto e depois usar o ddply para a análise final por país, mas os bancos ficam muito grandes
# com a multiplicação de rows nos dataframes da ELA. 



##### ANÁLISE CONGRUÊNCIA VOTERS - PARTY VOTED ##### 


###### BRASIL ######

bra_elavotes <- elavotes %>% filter (cname == "BRA")

bra_lapvoter <- lapvoter %>% filter (cname == "BRA")

###### WEIGHT POR VOTE SHARES#####
bra_elavotes$sample <- (bra_elavotes$votes/ bra_elavotes$interview) * 1000 

### REPETIÇÃO DE OBSERVAÇÕES POR WEIGHT:
bra_smp <- bra_elavotes[rep(seq(nrow(bra_elavotes)), bra_elavotes$sample),]


bra_smp$pop <- as.factor("y")
bra_lapvoter$pop <- as.factor("x")

### AJUSTES PARA CONGRUÊNCIA: 

bra_smp_cong <- bra_smp %>% dplyr::select (ROES2, pop) 
bra_lap_cong <- bra_lapvoter %>% dplyr::select (ROES2, pop)


bra_congvotes <-rbind(bra_smp_cong, bra_lap_cong)

bra_congvotes <- bra_congvotes %>% dplyr::rename (samps = ROES2) 

bra_congvotes$samps <- as.numeric(bra_congvotes$samps)

bra_congvotes <- completeFun(bra_congvotes, "samps")

# CONGRUÊNCIA
BRA_emd_votes_ROES2 <- emd.dis(bra_congvotes)
BRA_dmeans <- dmeans(bra_congvotes)


######BOLIVIA ####

bol_elavotes <- elavotes %>% filter (cname == "BOL")

bol_lapvoter <- lapvoter %>% filter (cname == "BOL")

###### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? #####
bol_elavotes$sample <- (bol_elavotes$votes/ bol_elavotes$interview) * 1000 
bol_smp <- bol_elavotes[rep(seq(nrow(bol_elavotes)), bol_elavotes$sample),]

bol_smp$pop <- as.factor("y")
bol_lapvoter$pop <- as.factor("x")

### Ajustes para congruência: 

bol_smp_cong <- bol_smp %>% select (ROES2, pop) 
bol_lap_cong <- bol_lapvoter %>% select (ROES2, pop)


bol_congvotes <-rbind(bol_smp_cong, bol_lap_cong)

bol_congvotes <- bol_congvotes %>% dplyr::rename (samps = ROES2) 

bol_congvotes$samps <- as.numeric(bol_congvotes$samps)

bol_congvotes <- completeFun(bol_congvotes, "samps")


BOL_emd_votes_ROES2 <- emd.dis(bol_congvotes)
BOL_dmeans <- dmeans (bol_congvotes)

##### CHILE #####

CHL_elavotes <- elavotes %>% filter (cname == "CHL")

CHL_lapvoter <- lapvoter %>% filter (cname == "CHL")

###### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? #####
CHL_elavotes$sample <- (CHL_elavotes$votes/ CHL_elavotes$interview) * 1000 
CHL_smp <- CHL_elavotes[rep(seq(nrow(CHL_elavotes)), CHL_elavotes$sample),]

CHL_smp$pop <- as.factor("y")
CHL_lapvoter$pop <- as.factor("x")

###### CONGRUÊNCIA - ROES 1 ##### 

CHL_smp_cong <- CHL_smp %>% select (ROES2, pop) 
CHL_lap_cong <- CHL_lapvoter %>% select (ROES2, pop)


CHL_congvotes <-rbind(CHL_smp_cong, CHL_lap_cong)

CHL_congvotes <- CHL_congvotes %>% dplyr::rename (samps = ROES2) 

CHL_congvotes$samps <- as.numeric(CHL_congvotes$samps)

CHL_congvotes <- completeFun(CHL_congvotes, "samps")


CHL_emd_votes_ROES2<- emd.dis(CHL_congvotes)
CHL_dmeans <-dmeans (CHL_congvotes)


##### COLÔMBIA #####


COL_elavotes <- elavotes %>% filter (cname == "COL")

COL_lapvoter <- lapvoter %>% filter (cname == "COL")

###### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? #####
COL_elavotes$sample <- (COL_elavotes$votes/ COL_elavotes$interview) * 1000 
COL_smp <- COL_elavotes[rep(seq(nrow(COL_elavotes)), COL_elavotes$sample),]

COL_smp$pop <- as.factor("y")
COL_lapvoter$pop <- as.factor("x")


COL_smp_cong <- COL_smp %>% select (ROES2, pop) 
COL_lap_cong <- COL_lapvoter %>% select (ROES2, pop)


COL_congvotes <-rbind(COL_smp_cong, COL_lap_cong)

COL_congvotes <- COL_congvotes %>% dplyr::rename (samps = ROES2) 

COL_congvotes$samps <- as.numeric(COL_congvotes$samps)

COL_congvotes <- completeFun(COL_congvotes, "samps")


COL_emd_votes_ROES2 <- emd.dis(COL_congvotes)
COL_dmeans  <- dmeans(COL_congvotes)


#### COSTA RICA ####

CRI_elavotes <- elavotes %>% filter (cname == "CRI")

CRI_lapvoter <- lapvoter %>% filter (cname == "CRI")

###### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? #####
CRI_elavotes$sample <- (CRI_elavotes$votes/ CRI_elavotes$interview) * 1000 
CRI_smp <- CRI_elavotes[rep(seq(nrow(CRI_elavotes)), CRI_elavotes$sample),]

CRI_smp$pop <- as.factor("y")
CRI_lapvoter$pop <- as.factor("x")


CRI_smp_cong <- CRI_smp %>% select (ROES2, pop) 
CRI_lap_cong <- CRI_lapvoter %>% select (ROES2, pop)


CRI_congvotes <-rbind(CRI_smp_cong, CRI_lap_cong)

CRI_congvotes <- CRI_congvotes %>% dplyr::rename (samps = ROES2) 

CRI_congvotes$samps <- as.numeric(CRI_congvotes$samps)

CRI_congvotes <- completeFun(CRI_congvotes, "samps")


CRI_emd_votes_ROES2 <-emd.dis(CRI_congvotes)
CRI_dmeans  <-dmeans (CRI_congvotes)


##### REP. DOMINICANA #####

DOM_elavotes <- elavotes %>% filter (cname == "DOM")

DOM_lapvoter <- lapvoter %>% filter (cname == "DOM")

###### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? #####
DOM_elavotes$sample <- (DOM_elavotes$votes/ DOM_elavotes$interview) * 1000 
DOM_smp <- DOM_elavotes[rep(seq(nrow(DOM_elavotes)), DOM_elavotes$sample),]

DOM_smp$pop <- as.factor("y")
DOM_lapvoter$pop <- as.factor("x")

DOM_smp_cong <- DOM_smp %>% select (ROES2, pop) 
DOM_lap_cong <- DOM_lapvoter %>% select (ROES2, pop)


DOM_congvotes <-rbind(DOM_smp_cong, DOM_lap_cong)

DOM_congvotes <- DOM_congvotes %>% dplyr::rename (samps = ROES2) 

DOM_congvotes$samps <- as.numeric(DOM_congvotes$samps)

DOM_congvotes <- completeFun(DOM_congvotes, "samps")


DOM_emd_votes_ROES2 <-emd.dis(DOM_congvotes)
DOM_dmeans  <-dmeans (DOM_congvotes)


##### GUATEMALA #####

GTM_elavotes <- elavotes %>% filter (cname == "GTM")

GTM_lapvoter <- lapvoter %>% filter (cname == "GTM")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
GTM_elavotes$sample <- (GTM_elavotes$votes/ GTM_elavotes$interview) * 1000 
GTM_smp <- GTM_elavotes[rep(seq(nrow(GTM_elavotes)), GTM_elavotes$sample),]

GTM_smp$pop <- as.factor("y")
GTM_lapvoter$pop <- as.factor("x")

### agora tentando a congruência:

GTM_smp_cong <- GTM_smp %>% select (ROES2, pop) 
GTM_lap_cong <- GTM_lapvoter %>% select (ROES2, pop)


GTM_congvotes <-rbind(GTM_smp_cong, GTM_lap_cong)

GTM_congvotes <- GTM_congvotes %>% dplyr::rename (samps = ROES2) 

GTM_congvotes$samps <- as.numeric(GTM_congvotes$samps)

GTM_congvotes <- completeFun(GTM_congvotes, "samps")

## CONGRUÊNCIA:

GTM_emd_votes_ROES2 <- emd.dis(GTM_congvotes)
GTM_dmeans  <-dmeans (GTM_congvotes)


##### HONDURAS #####

HND_elavotes <- elavotes %>% filter (cname == "HND")

HND_lapvoter <- lapvoter %>% filter (cname == "HND")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
HND_elavotes$sample <- (HND_elavotes$votes/ HND_elavotes$interview) * 1000 
HND_smp <- HND_elavotes[rep(seq(nrow(HND_elavotes)), HND_elavotes$sample),]

HND_smp$pop <- as.factor("y")
HND_lapvoter$pop <- as.factor("x")


HND_smp_cong <- HND_smp %>% select (ROES2, pop) 
HND_lap_cong <- HND_lapvoter %>% select (ROES2, pop)


HND_congvotes <-rbind(HND_smp_cong, HND_lap_cong)

HND_congvotes <- HND_congvotes %>% dplyr::rename (samps = ROES2) 

HND_congvotes$samps <- as.numeric(HND_congvotes$samps)

HND_congvotes <- completeFun(HND_congvotes, "samps")

## CONGRUÊNCIA:

HND_emd_votes_ROES2 <- emd.dis(HND_congvotes)
HND_dmeans  <- dmeans (HND_congvotes)


##### NICARAGUA #####

NIC_elavotes <- elavotes %>% filter (cname == "NIC")

NIC_lapvoter <- lapvoter %>% filter (cname == "NIC")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
NIC_elavotes$sample <- (NIC_elavotes$votes/ NIC_elavotes$interview) * 1000 
NIC_smp <- NIC_elavotes[rep(seq(nrow(NIC_elavotes)), NIC_elavotes$sample),]

NIC_smp$pop <- as.factor("y")
NIC_lapvoter$pop <- as.factor("x")


NIC_smp_cong <- NIC_smp %>% select (ROES2, pop) 
NIC_lap_cong <- NIC_lapvoter %>% select (ROES2, pop)


NIC_congvotes <-rbind(NIC_smp_cong, NIC_lap_cong)

NIC_congvotes <- NIC_congvotes %>% dplyr::rename (samps = ROES2) 

NIC_congvotes$samps <- as.numeric(NIC_congvotes$samps)

NIC_congvotes <- completeFun(NIC_congvotes, "samps")

## CONGRUÊNCIA:

NIC_emd_votes_ROES2 <- emd.dis(NIC_congvotes)
NIC_dmeans  <- dmeans (NIC_congvotes)


##### URUGUAY #####

URY_elavotes <- elavotes %>% filter (cname == "URY")

URY_lapvoter <- lapvoter %>% filter (cname == "URY")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
URY_elavotes$sample <- (URY_elavotes$votes/ URY_elavotes$interview) * 1000 
URY_smp <- URY_elavotes[rep(seq(nrow(URY_elavotes)), URY_elavotes$sample),]

URY_smp$pop <- as.factor("y")
URY_lapvoter$pop <- as.factor("x")


URY_smp_cong <- URY_smp %>% select (ROES2, pop) 
URY_lap_cong <- URY_lapvoter %>% select (ROES2, pop)


URY_congvotes <-rbind(URY_smp_cong, URY_lap_cong)

URY_congvotes <- URY_congvotes %>% dplyr::rename (samps = ROES2) 

URY_congvotes$samps <- as.numeric(URY_congvotes$samps)

URY_congvotes <- completeFun(URY_congvotes, "samps")

## CONGRUÊNCIA:

URY_emd_votes_ROES2 <- emd.dis(URY_congvotes)
URY_dmeans  <- dmeans(URY_congvotes)



write.xlsx(list(BOL =BOL_emd_votes_ROES2, BRA =BRA_emd_votes_ROES2, CHL=CHL_emd_votes_ROES2, 
                COL=COL_emd_votes_ROES2, CRI=CRI_emd_votes_ROES2, DOM=DOM_emd_votes_ROES2, GTM=GTM_emd_votes_ROES2, HND=HND_emd_votes_ROES2, 
                NIC=NIC_emd_votes_ROES2, URY=URY_emd_votes_ROES2),
           "EMD - Voter_votes - ROES2.xlsx")
