library(xlsx)

##### CRIANDO FUNÇÃO PARA TIRAR NA #####
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

setwd ("C:/Users/livia/Desktop/ISSUES") 

### BAIXANDO OS BANCOS E FAZENDO ALGUMAS ALTERAÇÕES INICIAIS

names (elavotes) <- gsub("ROES10", "ROES", names(elavotes)) 


lapcitizen <- completeFun(lap_emd, c("voted", "ROES1"))

elaroes1 <- completeFun(elavotes, c("votes", "seats", "ROES1"))

#Subset de eleitores:

lapvoter <- lapcitizen %>% 
  filter (voted==1)


### Só consegui fazer a análise em separado de todos países, poderia fazer tudo isso 
# em conjunto e depois usar o ddply para a análise final por país, mas os bancos ficam muito grandes
# com a multiplicação de rows nos dataframes da ELA. 


##### ANÁLISE CONGRUÊNCIA VOTERS - PARTY VOTED ##### 

##### ARGENTINA #####

arg_elaroes1 <- elaroes1 %>% filter (cname == "ARG")

arg_lapvoter <- lapvoter %>% filter (cname == "ARG")

# VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? 
arg_elaroes1$sample <- (arg_elaroes1$votes/ arg_elaroes1$interview) * 1000 
arg_smp <- arg_elaroes1[rep(seq(nrow(arg_elaroes1)), arg_elaroes1$sample),]

arg_smp$pop <- as.factor("y")
arg_lapvoter$pop <- as.factor("x")

### Ajustes para congruência: 

arg_smp_cong <- arg_smp %>% select (ROES1, pop) 
arg_lap_cong <- arg_lapvoter %>% select (ROES1, pop)


arg_congvotes <-rbind(arg_smp_cong, arg_lap_cong)

arg_congvotes <- arg_congvotes %>% dplyr::rename (samps = ROES1) 

arg_congvotes$samps <- as.numeric(arg_congvotes$samps)

arg_congvotes <- completeFun(arg_congvotes, "samps")


ARG_emd_votes_ROES1 <- emd.dis(arg_congvotes)
ARG_dmeans <- dmeans (arg_congvotes)


######BOLIVIA ####

bol_elaroes1 <- elaroes1 %>% filter (cname == "BOL")

bol_lapvoter <- lapvoter %>% filter (cname == "BOL")

# VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? 
bol_elaroes1$sample <- (bol_elaroes1$votes/ bol_elaroes1$interview) * 1000 
bol_smp <- bol_elaroes1[rep(seq(nrow(bol_elaroes1)), bol_elaroes1$sample),]

bol_smp$pop <- as.factor("y")
bol_lapvoter$pop <- as.factor("x")

### Ajustes para congruência: 

bol_smp_cong <- bol_smp %>% select (ROES1, pop) 
bol_lap_cong <- bol_lapvoter %>% select (ROES1, pop)


bol_congvotes <-rbind(bol_smp_cong, bol_lap_cong)

bol_congvotes <- bol_congvotes %>% dplyr::rename (samps = ROES1) 

bol_congvotes$samps <- as.numeric(bol_congvotes$samps)

bol_congvotes <- completeFun(bol_congvotes, "samps")


BOL_emd_votes_ROES1 <- emd.dis(bol_congvotes)
BOL_dmeans <- dmeans (bol_congvotes)



###### BRASIL ######

bra_elaroes1 <- elaroes1 %>% filter (cname == "BRA")

bra_lapvoter <- lapvoter %>% filter (cname == "BRA")

# WEIGHT POR VOTE SHARES
bra_elaroes1$sample <- (bra_elaroes1$votes/ bra_elaroes1$interview) * 1000 

### REPETIÇÃO DE OBSERVAÇÕES POR WEIGHT:
bra_smp <- bra_elaroes1[rep(seq(nrow(bra_elaroes1)), bra_elaroes1$sample),]


bra_smp$pop <- as.factor("y")
bra_lapvoter$pop <- as.factor("x")

### AJUSTES PARA CONGRUÊNCIA: 

bra_smp_cong <- bra_smp %>% dplyr::select (ROES1, pop) 
bra_lap_cong <- bra_lapvoter %>% dplyr::select (ROES1, pop)


bra_congvotes <-rbind(bra_smp_cong, bra_lap_cong)

bra_congvotes <- bra_congvotes %>% dplyr::rename (samps = ROES1) 

bra_congvotes$samps <- as.numeric(bra_congvotes$samps)

bra_congvotes <- completeFun(bra_congvotes, "samps")

# CONGRUÊNCIA
BRA_emd_votes_ROES1 <- emd.dis(bra_congvotes)
BRA_dmeans <- dmeans(bra_congvotes)


##### CHILE #####

CHL_elaroes1 <- elaroes1 %>% filter (cname == "CHL")

CHL_lapvoter <- lapvoter %>% filter (cname == "CHL")

CHL_elaroes1$sample <- (CHL_elaroes1$votes/ CHL_elaroes1$interview) * 1000 
CHL_smp <- CHL_elaroes1[rep(seq(nrow(CHL_elaroes1)), CHL_elaroes1$sample),]

CHL_smp$pop <- as.factor("y")
CHL_lapvoter$pop <- as.factor("x")


CHL_smp_cong <- CHL_smp %>% select (ROES1, pop) 
CHL_lap_cong <- CHL_lapvoter %>% select (ROES1, pop)


CHL_congvotes <-rbind(CHL_smp_cong, CHL_lap_cong)

CHL_congvotes <- CHL_congvotes %>% dplyr::rename (samps = ROES1) 

CHL_congvotes$samps <- as.numeric(CHL_congvotes$samps)

CHL_congvotes <- completeFun(CHL_congvotes, "samps")


CHL_emd_votes_ROES1<- emd.dis(CHL_congvotes)
CHL_dmeans <-dmeans (CHL_congvotes)



#### COSTA RICA ####

CRI_elaroes1 <- elaroes1 %>% filter (cname == "CRI")

CRI_lapvoter <- lapvoter %>% filter (cname == "CRI")

CRI_elaroes1$sample <- (CRI_elaroes1$votes/ CRI_elaroes1$interview) * 1000 
CRI_smp <- CRI_elaroes1[rep(seq(nrow(CRI_elaroes1)), CRI_elaroes1$sample),]

CRI_smp$pop <- as.factor("y")
CRI_lapvoter$pop <- as.factor("x")


CRI_smp_cong <- CRI_smp %>% select (ROES1, pop) 
CRI_lap_cong <- CRI_lapvoter %>% select (ROES1, pop)


CRI_congvotes <-rbind(CRI_smp_cong, CRI_lap_cong)

CRI_congvotes <- CRI_congvotes %>% dplyr::rename (samps = ROES1) 

CRI_congvotes$samps <- as.numeric(CRI_congvotes$samps)

CRI_congvotes <- completeFun(CRI_congvotes, "samps")


CRI_emd_votes_ROES1 <-emd.dis(CRI_congvotes)
CRI_dmeans  <-dmeans (CRI_congvotes)


##### ECUADOR #####

ecu_elaroes1 <- elaroes1 %>% filter (cname == "ECU")

ecu_lapvoter <- lapvoter %>% filter (cname == "ECU")

# VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? 
ecu_elaroes1$sample <- (ecu_elaroes1$votes/ ecu_elaroes1$interview) * 1000 
ecu_smp <- ecu_elaroes1[rep(seq(nrow(ecu_elaroes1)), ecu_elaroes1$sample),]

ecu_smp$pop <- as.factor("y")
ecu_lapvoter$pop <- as.factor("x")

### Ajustes para congruência: 

ecu_smp_cong <- ecu_smp %>% select (ROES1, pop) 
ecu_lap_cong <- ecu_lapvoter %>% select (ROES1, pop)


ecu_congvotes <-rbind(ecu_smp_cong, ecu_lap_cong)

ecu_congvotes <- ecu_congvotes %>% dplyr::rename (samps = ROES1) 

ecu_congvotes$samps <- as.numeric(ecu_congvotes$samps)

ecu_congvotes <- completeFun(ecu_congvotes, "samps")


ECU_emd_votes_ROES1 <- emd.dis(ecu_congvotes)
ECU_dmeans <- dmeans (ecu_congvotes)



##### GUATEMALA #####

GTM_elaroes1 <- elaroes1 %>% filter (cname == "GTM")

GTM_lapvoter <- lapvoter %>% filter (cname == "GTM")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
GTM_elaroes1$sample <- (GTM_elaroes1$votes/ GTM_elaroes1$interview) * 1000 
GTM_smp <- GTM_elaroes1[rep(seq(nrow(GTM_elaroes1)), GTM_elaroes1$sample),]

GTM_smp$pop <- as.factor("y")
GTM_lapvoter$pop <- as.factor("x")

### agora tentando a congruência:

GTM_smp_cong <- GTM_smp %>% select (ROES1, pop) 
GTM_lap_cong <- GTM_lapvoter %>% select (ROES1, pop)


GTM_congvotes <-rbind(GTM_smp_cong, GTM_lap_cong)

GTM_congvotes <- GTM_congvotes %>% dplyr::rename (samps = ROES1) 

GTM_congvotes$samps <- as.numeric(GTM_congvotes$samps)

GTM_congvotes <- completeFun(GTM_congvotes, "samps")

## CONGRUÊNCIA:

GTM_emd_votes_ROES1 <- emd.dis(GTM_congvotes)
GTM_dmeans  <-dmeans (GTM_congvotes)


##### HONDURAS #####

HND_elaroes1 <- elaroes1 %>% filter (cname == "HND")

HND_lapvoter <- lapvoter %>% filter (cname == "HND")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
HND_elaroes1$sample <- (HND_elaroes1$votes/ HND_elaroes1$interview) * 1000 
HND_smp <- HND_elaroes1[rep(seq(nrow(HND_elaroes1)), HND_elaroes1$sample),]

HND_smp$pop <- as.factor("y")
HND_lapvoter$pop <- as.factor("x")


HND_smp_cong <- HND_smp %>% select (ROES1, pop) 
HND_lap_cong <- HND_lapvoter %>% select (ROES1, pop)


HND_congvotes <-rbind(HND_smp_cong, HND_lap_cong)

HND_congvotes <- HND_congvotes %>% dplyr::rename (samps = ROES1) 

HND_congvotes$samps <- as.numeric(HND_congvotes$samps)

HND_congvotes <- completeFun(HND_congvotes, "samps")

## CONGRUÊNCIA:

HND_emd_votes_ROES1 <- emd.dis(HND_congvotes)
HND_dmeans  <- dmeans (HND_congvotes)



##### MEXICO #####

mex_elaroes1 <- elaroes1 %>% filter (cname == "MEX")

mex_lapvoter <- lapvoter %>% filter (cname == "MEX")

# VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? 
mex_elaroes1$sample <- (mex_elaroes1$votes/ mex_elaroes1$interview) * 1000 
mex_smp <- mex_elaroes1[rep(seq(nrow(mex_elaroes1)), mex_elaroes1$sample),]

mex_smp$pop <- as.factor("y")
mex_lapvoter$pop <- as.factor("x")

### Ajustes para congruência: 

mex_smp_cong <- mex_smp %>% select (ROES1, pop) 
mex_lap_cong <- mex_lapvoter %>% select (ROES1, pop)


mex_congvotes <-rbind(mex_smp_cong, mex_lap_cong)

mex_congvotes <- mex_congvotes %>% dplyr::rename (samps = ROES1) 

mex_congvotes$samps <- as.numeric(mex_congvotes$samps)

mex_congvotes <- completeFun(mex_congvotes, "samps")


MEX_emd_votes_ROES1 <- emd.dis(mex_congvotes)
MEX_dmeans <- dmeans (mex_congvotes)



##### NICARAGUA #####

NIC_elaroes1 <- elaroes1 %>% filter (cname == "NIC")

NIC_lapvoter <- lapvoter %>% filter (cname == "NIC")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
NIC_elaroes1$sample <- (NIC_elaroes1$votes/ NIC_elaroes1$interview) * 1000 
NIC_smp <- NIC_elaroes1[rep(seq(nrow(NIC_elaroes1)), NIC_elaroes1$sample),]

NIC_smp$pop <- as.factor("y")
NIC_lapvoter$pop <- as.factor("x")


NIC_smp_cong <- NIC_smp %>% select (ROES1, pop) 
NIC_lap_cong <- NIC_lapvoter %>% select (ROES1, pop)


NIC_congvotes <-rbind(NIC_smp_cong, NIC_lap_cong)

NIC_congvotes <- NIC_congvotes %>% dplyr::rename (samps = ROES1) 

NIC_congvotes$samps <- as.numeric(NIC_congvotes$samps)

NIC_congvotes <- completeFun(NIC_congvotes, "samps")

## CONGRUÊNCIA:

NIC_emd_votes_ROES1 <- emd.dis(NIC_congvotes)
NIC_dmeans  <- dmeans (NIC_congvotes)


##### PANAMA #####


pan_elaroes1 <- elaroes1 %>% filter (cname == "PAN")

pan_lapvoter <- lapvoter %>% filter (cname == "PAN")

# VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? 
pan_elaroes1$sample <- (pan_elaroes1$votes/ pan_elaroes1$interview) * 1000 
pan_smp <- pan_elaroes1[rep(seq(nrow(pan_elaroes1)), pan_elaroes1$sample),]

pan_smp$pop <- as.factor("y")
pan_lapvoter$pop <- as.factor("x")

### Ajustes para congruência: 

pan_smp_cong <- pan_smp %>% select (ROES1, pop) 
pan_lap_cong <- pan_lapvoter %>% select (ROES1, pop)


pan_congvotes <-rbind(pan_smp_cong, pan_lap_cong)

pan_congvotes <- pan_congvotes %>% dplyr::rename (samps = ROES1) 

pan_congvotes$samps <- as.numeric(pan_congvotes$samps)

pan_congvotes <- completeFun(pan_congvotes, "samps")


PAN_emd_votes_ROES1 <- emd.dis(pan_congvotes)
PAN_dmeans <- dmeans (pan_congvotes)


##### PERU #####


per_elaroes1 <- elaroes1 %>% filter (cname == "PER")

per_lapvoter <- lapvoter %>% filter (cname == "PER")

# VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? 
per_elaroes1$sample <- (per_elaroes1$votes/ per_elaroes1$interview) * 1000 
per_smp <- per_elaroes1[rep(seq(nrow(per_elaroes1)), per_elaroes1$sample),]

per_smp$pop <- as.factor("y")
per_lapvoter$pop <- as.factor("x")

### Ajustes para congruência: 

per_smp_cong <- per_smp %>% select (ROES1, pop) 
per_lap_cong <- per_lapvoter %>% select (ROES1, pop)


per_congvotes <-rbind(per_smp_cong, per_lap_cong)

per_congvotes <- per_congvotes %>% dplyr::rename (samps = ROES1) 

per_congvotes$samps <- as.numeric(per_congvotes$samps)

per_congvotes <- completeFun(per_congvotes, "samps")


PER_emd_votes_ROES1 <- emd.dis(per_congvotes)
PER_dmeans <- dmeans (per_congvotes)



##### PARAGUAY #####

pry_elaroes1 <- elaroes1 %>% filter (cname == "PRY")

pry_lapvoter <- lapvoter %>% filter (cname == "PRY")

# VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? 
pry_elaroes1$sample <- (pry_elaroes1$votes/ pry_elaroes1$interview) * 1000 
pry_smp <- pry_elaroes1[rep(seq(nrow(pry_elaroes1)), pry_elaroes1$sample),]

pry_smp$pop <- as.factor("y")
pry_lapvoter$pop <- as.factor("x")

### Ajustes para congruência: 

pry_smp_cong <- pry_smp %>% select (ROES1, pop) 
pry_lap_cong <- pry_lapvoter %>% select (ROES1, pop)


pry_congvotes <-rbind(pry_smp_cong, pry_lap_cong)

pry_congvotes <- pry_congvotes %>% dplyr::rename (samps = ROES1) 

pry_congvotes$samps <- as.numeric(pry_congvotes$samps)

pry_congvotes <- completeFun(pry_congvotes, "samps")


PRY_emd_votes_ROES1 <- emd.dis(pry_congvotes)
PRY_dmeans <- dmeans (pry_congvotes)


##### EL SALVADOR #####

slv_elaroes1 <- elaroes1 %>% filter (cname == "SLV")

slv_lapvoter <- lapvoter %>% filter (cname == "SLV")

# VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? 
slv_elaroes1$sample <- (slv_elaroes1$votes/ slv_elaroes1$interview) * 1000 
slv_smp <- slv_elaroes1[rep(seq(nrow(slv_elaroes1)), slv_elaroes1$sample),]

slv_smp$pop <- as.factor("y")
slv_lapvoter$pop <- as.factor("x")

### Ajustes para congruência: 

slv_smp_cong <- slv_smp %>% select (ROES1, pop) 
slv_lap_cong <- slv_lapvoter %>% select (ROES1, pop)


slv_congvotes <-rbind(slv_smp_cong, slv_lap_cong)

slv_congvotes <- slv_congvotes %>% dplyr::rename (samps = ROES1) 

slv_congvotes$samps <- as.numeric(slv_congvotes$samps)

slv_congvotes <- completeFun(slv_congvotes, "samps")


SLV_emd_votes_ROES1 <- emd.dis(slv_congvotes)
SLV_dmeans <- dmeans (slv_congvotes)

##### URUGUAY #####

URY_elaroes1 <- elaroes1 %>% filter (cname == "URY")

URY_lapvoter <- lapvoter %>% filter (cname == "URY")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
URY_elaroes1$sample <- (URY_elaroes1$votes/ URY_elaroes1$interview) * 1000 
URY_smp <- URY_elaroes1[rep(seq(nrow(URY_elaroes1)), URY_elaroes1$sample),]

URY_smp$pop <- as.factor("y")
URY_lapvoter$pop <- as.factor("x")


URY_smp_cong <- URY_smp %>% select (ROES1, pop) 
URY_lap_cong <- URY_lapvoter %>% select (ROES1, pop)


URY_congvotes <-rbind(URY_smp_cong, URY_lap_cong)

URY_congvotes <- URY_congvotes %>% dplyr::rename (samps = ROES1) 

URY_congvotes$samps <- as.numeric(URY_congvotes$samps)

URY_congvotes <- completeFun(URY_congvotes, "samps")

## CONGRUÊNCIA:

URY_emd_votes_ROES1 <- emd.dis(URY_congvotes)
URY_dmeans  <- dmeans(URY_congvotes)



write.xlsx(list(BOL =BOL_emd_votes_ROES1, BRA =BRA_emd_votes_ROES1, CHL=CHL_emd_votes_ROES1, 
                COL=COL_emd_votes_ROES1, CRI=CRI_emd_votes_ROES1, DOM=DOM_emd_votes_ROES1, GTM=GTM_emd_votes_ROES1, HND=HND_emd_votes_ROES1, 
                NIC=NIC_emd_votes_ROES1, URY=URY_emd_votes_ROES1),
           "EMD - Voter_votes - ROES1.xlsx")
