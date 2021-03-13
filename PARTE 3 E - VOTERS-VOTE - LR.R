library(xlsx)
library(dplyr)
library(emdist)
library(haven)
library(readxl)

##### CRIANDO FUNÇÃO PARA TIRAR ROWS COM MISSING DE UMA VARIÁVEL APENAS
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


### BAIXANDO OS BANCOS E FAZENDO ALGUMAS ALTERAÇÕES INICIAIS

load ("lapfinal.RData")

elavotes <- read_xlsx("ela_voteclean.xlsx")

elavotes <- elavotes %>% dplyr::rename (same_sex = VAL1, ideol_self = ID1)


##### ANÁLISE CONGRUÊNCIA VOTERS - PARTY VOTED - LEFT-RIGHT ##### 

lapcitizen <- completeFun(lapfinal, c("vb2", "ideol_self"))

elavotes <- completeFun(elavotes, "ideol_self")


#Fazer subset de eleitores:

lapvoter <- lapcitizen %>% 
  filter (vb2==1)

######BRASIL ####

BRA_elavotes <- elavotes %>% filter (cname == "BRA")

BRA_lapvoter <- lapvoter %>% filter (cname == "BRA")

###### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? #####
BRA_elavotes$sample <- (BRA_elavotes$votes/ BRA_elavotes$interview) * 1000 
BRA_smp <- BRA_elavotes[rep(seq(nrow(BRA_elavotes)), BRA_elavotes$sample),]

BRA_smp$pop <- as.factor("y")
BRA_lapvoter$pop <- as.factor("x")

### agora tentando a congruência:

BRA_smp_cong <- BRA_smp %>% select (ideol_self, pop) 
BRA_lap_cong <- BRA_lapvoter %>% select (ideol_self, pop)


BRA_congvotes <-rbind(BRA_smp_cong, BRA_lap_cong)

BRA_congvotes <- BRA_congvotes %>% dplyr::rename (samps = ideol_self) 

BRA_congvotes$samps <- as.numeric(BRA_congvotes$samps)

BRA_congvotes <- completeFun(BRA_congvotes, "samps")


######BOLIVIA ####

BOL_elavotes <- elavotes %>% filter (cname == "BOL")

BOL_lapvoter <- lapvoter %>% filter (cname == "BOL")

###### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? #####
BOL_elavotes$sample <- (BOL_elavotes$votes/ BOL_elavotes$interview) * 1000 
BOL_smp <- BOL_elavotes[rep(seq(nrow(BOL_elavotes)), BOL_elavotes$sample),]

BOL_smp$pop <- as.factor("y")
BOL_lapvoter$pop <- as.factor("x")

### agora tentando a congruência:

BOL_smp_cong <- BOL_smp %>% select (ideol_self, pop) 
BOL_lap_cong <- BOL_lapvoter %>% select (ideol_self, pop)


BOL_congvotes <-rbind(BOL_smp_cong, BOL_lap_cong)

BOL_congvotes <- BOL_congvotes %>% dplyr::rename (samps = ideol_self) 

BOL_congvotes$samps <- as.numeric(BOL_congvotes$samps)

BOL_congvotes <- completeFun(BOL_congvotes, "samps")



##### CHILE #####

CHL_elavotes <- elavotes %>% filter (cname == "CHL")

CHL_lapvoter <- lapvoter %>% filter (cname == "CHL")

###### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? #####
CHL_elavotes$sample <- (CHL_elavotes$votes/ CHL_elavotes$interview) * 1000 
CHL_smp <- CHL_elavotes[rep(seq(nrow(CHL_elavotes)), CHL_elavotes$sample),]

CHL_smp$pop <- as.factor("y")
CHL_lapvoter$pop <- as.factor("x")

### agora tentando a congruência:

CHL_smp_cong <- CHL_smp %>% select (ideol_self, pop) 
CHL_lap_cong <- CHL_lapvoter %>% select (ideol_self, pop)


CHL_congvotes <-rbind(CHL_smp_cong, CHL_lap_cong)

CHL_congvotes <- CHL_congvotes %>% dplyr::rename (samps = ideol_self) 

CHL_congvotes$samps <- as.numeric(CHL_congvotes$samps)

CHL_congvotes <- completeFun(CHL_congvotes, "samps")


##### COLÔMBIA #####


COL_elavotes <- elavotes %>% filter (cname == "COL")

COL_lapvoter <- lapvoter %>% filter (cname == "COL")

###### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? #####
COL_elavotes$sample <- (COL_elavotes$votes/ COL_elavotes$interview) * 1000 
COL_smp <- COL_elavotes[rep(seq(nrow(COL_elavotes)), COL_elavotes$sample),]

COL_smp$pop <- as.factor("y")
COL_lapvoter$pop <- as.factor("x")

### agora tentando a congruência:

COL_smp_cong <- COL_smp %>% select (ideol_self, pop) 
COL_lap_cong <- COL_lapvoter %>% select (ideol_self, pop)


COL_congvotes <-rbind(COL_smp_cong, COL_lap_cong)

COL_congvotes <- COL_congvotes %>% dplyr::rename (samps = ideol_self) 

COL_congvotes$samps <- as.numeric(COL_congvotes$samps)

COL_congvotes <- completeFun(COL_congvotes, "samps")



#### COSTA RICA ####

CRI_elavotes <- elavotes %>% filter (cname == "CRI")

CRI_lapvoter <- lapvoter %>% filter (cname == "CRI")

###### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? #####
CRI_elavotes$sample <- (CRI_elavotes$votes/ CRI_elavotes$interview) * 1000 
CRI_smp <- CRI_elavotes[rep(seq(nrow(CRI_elavotes)), CRI_elavotes$sample),]

CRI_smp$pop <- as.factor("y")
CRI_lapvoter$pop <- as.factor("x")

### ALTERAÇÕES PARA CONGRUÊNCIA

CRI_smp_cong <- CRI_smp %>% select (ideol_self, pop) 
CRI_lap_cong <- CRI_lapvoter %>% select (ideol_self, pop)


CRI_congvotes <-rbind(CRI_smp_cong, CRI_lap_cong)

CRI_congvotes <- CRI_congvotes %>% dplyr::rename (samps = ideol_self) 

CRI_congvotes$samps <- as.numeric(CRI_congvotes$samps)

CRI_congvotes <- completeFun(CRI_congvotes, "samps")



##### REP. DOMINICANA #####

DOM_elavotes <- elavotes %>% filter (cname == "DOM")

DOM_lapvoter <- lapvoter %>% filter (cname == "DOM")

###### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000? #####
DOM_elavotes$sample <- (DOM_elavotes$votes/ DOM_elavotes$interview) * 1000 
DOM_smp <- DOM_elavotes[rep(seq(nrow(DOM_elavotes)), DOM_elavotes$sample),]

DOM_smp$pop <- as.factor("y")
DOM_lapvoter$pop <- as.factor("x")

### agora tentando a congruência:

DOM_smp_cong <- DOM_smp %>% select (ideol_self, pop) 
DOM_lap_cong <- DOM_lapvoter %>% select (ideol_self, pop)


DOM_congvotes <-rbind(DOM_smp_cong, DOM_lap_cong)

DOM_congvotes <- DOM_congvotes %>% dplyr::rename (samps = ideol_self) 

DOM_congvotes$samps <- as.numeric(DOM_congvotes$samps)

DOM_congvotes <- completeFun(DOM_congvotes, "samps")






##### GUATEMALA #####

GTM_elavotes <- elavotes %>% filter (cname == "GTM")

GTM_lapvoter <- lapvoter %>% filter (cname == "GTM")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
GTM_elavotes$sample <- (GTM_elavotes$votes/ GTM_elavotes$interview) * 1000 
GTM_smp <- GTM_elavotes[rep(seq(nrow(GTM_elavotes)), GTM_elavotes$sample),]

GTM_smp$pop <- as.factor("y")
GTM_lapvoter$pop <- as.factor("x")

### agora tentando a congruência:

GTM_smp_cong <- GTM_smp %>% select (ideol_self, pop) 
GTM_lap_cong <- GTM_lapvoter %>% select (ideol_self, pop)


GTM_congvotes <-rbind(GTM_smp_cong, GTM_lap_cong)

GTM_congvotes <- GTM_congvotes %>% dplyr::rename (samps = ideol_self) 

GTM_congvotes$samps <- as.numeric(GTM_congvotes$samps)

GTM_congvotes <- completeFun(GTM_congvotes, "samps")



##### HONDURAS #####

HND_elavotes <- elavotes %>% filter (cname == "HND")

HND_lapvoter <- lapvoter %>% filter (cname == "HND")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
HND_elavotes$sample <- (HND_elavotes$votes/ HND_elavotes$interview) * 1000 
HND_smp <- HND_elavotes[rep(seq(nrow(HND_elavotes)), HND_elavotes$sample),]

HND_smp$pop <- as.factor("y")
HND_lapvoter$pop <- as.factor("x")

### agora tentando a congruência:

HND_smp_cong <- HND_smp %>% select (ideol_self, pop) 
HND_lap_cong <- HND_lapvoter %>% select (ideol_self, pop)


HND_congvotes <-rbind(HND_smp_cong, HND_lap_cong)

HND_congvotes <- HND_congvotes %>% dplyr::rename (samps = ideol_self) 

HND_congvotes$samps <- as.numeric(HND_congvotes$samps)

HND_congvotes <- completeFun(HND_congvotes, "samps")




##### NICARAGUA #####

NIC_elavotes <- elavotes %>% filter (cname == "NIC")

NIC_lapvoter <- lapvoter %>% filter (cname == "NIC")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
NIC_elavotes$sample <- (NIC_elavotes$votes/ NIC_elavotes$interview) * 1000 
NIC_smp <- NIC_elavotes[rep(seq(nrow(NIC_elavotes)), NIC_elavotes$sample),]

NIC_smp$pop <- as.factor("y")
NIC_lapvoter$pop <- as.factor("x")

### agora tentando a congruência:

NIC_smp_cong <- NIC_smp %>% select (ideol_self, pop) 
NIC_lap_cong <- NIC_lapvoter %>% select (ideol_self, pop)


NIC_congvotes <-rbind(NIC_smp_cong, NIC_lap_cong)

NIC_congvotes <- NIC_congvotes %>% dplyr::rename (samps = ideol_self) 

NIC_congvotes$samps <- as.numeric(NIC_congvotes$samps)

NIC_congvotes <- completeFun(NIC_congvotes, "samps")




##### URUGUAY #####

URY_elavotes <- elavotes %>% filter (cname == "URY")

URY_lapvoter <- lapvoter %>% filter (cname == "URY")

### VARIÁVEL PARA SHARES : SHARES / SEATS * 1000
URY_elavotes$sample <- (URY_elavotes$votes/ URY_elavotes$interview) * 1000 
URY_smp <- URY_elavotes[rep(seq(nrow(URY_elavotes)), URY_elavotes$sample),]

URY_smp$pop <- as.factor("y")
URY_lapvoter$pop <- as.factor("x")

### agora tentando a congruência:

URY_smp_cong <- URY_smp %>% select (ideol_self, pop) 
URY_lap_cong <- URY_lapvoter %>% select (ideol_self, pop)


URY_congvotes <-rbind(URY_smp_cong, URY_lap_cong)

URY_congvotes <- URY_congvotes %>% dplyr::rename (samps = ideol_self) 

URY_congvotes$samps <- as.numeric(URY_congvotes$samps)

URY_congvotes <- completeFun(URY_congvotes, "samps")

###### MEDIDAS DE CONGRUÊNCIA #####


## EMD: 

BRA_emd_vote_sm <- emd.dis(BRA_congvotes)
BOL_emd_vote_sm <- emd.dis(BOL_congvotes)
CHL_emd_vote_sm <- emd.dis(CHL_congvotes)
COL_emd_vote_sm <- emd.dis(COL_congvotes)
CRI_emd_vote_sm <- emd.dis(CRI_congvotes)
DOM_emd_vote_sm <- emd.dis(DOM_congvotes)
GTM_emd_vote_sm <- emd.dis(GTM_congvotes)
HND_emd_vote_sm <- emd.dis(HND_congvotes)
NIC_emd_vote_sm <- emd.dis(NIC_congvotes)
URY_emd_vote_sm <- emd.dis(URY_congvotes)



###### CDF Overlap #####


BRA_cdf_vote_sm <- cdf.overlap(BRA_congvotes)
BOL_cdf_vote_sm <- cdf.overlap(BOL_congvotes)
CHL_cdf_vote_sm <- cdf.overlap(CHL_congvotes)
COL_cdf_vote_sm <- cdf.overlap(COL_congvotes)
CRI_cdf_vote_sm <- cdf.overlap(CRI_congvotes)
DOM_cdf_vote_sm <- cdf.overlap(DOM_congvotes)
GTM_cdf_vote_sm <- cdf.overlap(GTM_congvotes)
HND_cdf_vote_sm <- cdf.overlap(HND_congvotes)
NIC_cdf_vote_sm <- cdf.overlap(NIC_congvotes)
URY_cdf_vote_sm <- cdf.overlap(URY_congvotes)


##### MEANS #####

BRA_means_vote_sm <- dmeans(BRA_congvotes)
BOL_means_vote_sm <- dmeans(BOL_congvotes)
CHL_means_vote_sm <- dmeans(CHL_congvotes)
COL_means_vote_sm <- dmeans(COL_congvotes)
CRI_means_vote_sm <- dmeans(CRI_congvotes)
DOM_means_vote_sm <- dmeans(DOM_congvotes)
GTM_means_vote_sm <- dmeans(GTM_congvotes)
HND_means_vote_sm <- dmeans(HND_congvotes)
NIC_means_vote_sm <- dmeans(NIC_congvotes)
URY_means_vote_sm <- dmeans(URY_congvotes)

##### TABELA DE RESULTADOS #####

tab <- cbind(c(BOL_means_vote_sm,	BRA_means_vote_sm, CHL_means_vote_sm,
               COL_means_vote_sm, CRI_means_vote_sm, DOM_means_vote_sm,
               GTM_means_vote_sm, HND_means_vote_sm, NIC_means_vote_sm,	        
               URY_means_vote_sm), 
             c(BOL_emd_vote_sm, BRA_emd_vote_sm, CHL_emd_vote_sm,
               COL_emd_vote_sm, CRI_emd_vote_sm, DOM_emd_vote_sm,
               GTM_emd_vote_sm, HND_emd_vote_sm, NIC_emd_vote_sm,	        
               URY_emd_vote_sm),
             c(BOL_cdf_vote_sm,	BRA_cdf_vote_sm, CHL_cdf_vote_sm,
               COL_cdf_vote_sm, CRI_cdf_vote_sm, DOM_cdf_vote_sm,
               GTM_cdf_vote_sm, HND_cdf_vote_sm, NIC_cdf_vote_sm,	        
               URY_cdf_vote_sm)) 


dimnames(tab) <- list(c("BOLIVIA","BRAZIL", "CHILE", "COLOMBIA", "COSTA RICA", "REP DOM", "GUATEMALA", 
                        "HONDURAS", "NICARAGUA", "URUGUAY"),
                      c("Diff in Means","EMD", "CDF overlap"))


write.xlsx(list(BOL =BOL_emd_vote_sm, BRA =BRA_emd_vote_sm, CHL=CHL_emd_vote_sm, 
                COL=COL_emd_vote_sm, CRI=CRI_emd_vote_sm, DOM=DOM_emd_vote_sm, GTM=GTM_emd_vote_sm, HND=HND_emd_vote_sm, 
                NIC=NIC_emd_vote_sm, URY=URY_emd_vote_sm),
           "EMD - Voter_votes - LR.xlsx")

stargazer(tab, title = "Voters-to-Votes Congruence - LEFT-RIGHT")



