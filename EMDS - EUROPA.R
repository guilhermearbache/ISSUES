##### PACKAGES AND FUNCTIONS #####

library(emdist)
library(plyr)

#TIRAR MISSINGS
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
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


##### PREPARING DATASETS #####

 
# #EURO_CAND - FILTRAR ELEITOS, VALIDOS
# 
# 
# onlycand_same_sex <- cand_same_sex %>% filter (mep ==2)                       
# 
# mep_same_sex<- cand_same_sex %>% filter (mep == 1)                       


#SELECIONAR VAR INTERESSE 
candidates <- euro_cand %>% select (country, ptyname_eng, votes, seats, lsq,
                                    v020_03, v020_04, v020_08, v018_1, v020_05, v020_02) %>%
                            
                                    
citizen <- euro_voter %>% select (country, q24, q58,q59, q63, q46, q60, q57) # Pelo nome de país parece que o matching
#do ddply está dando certo, posso testar depois com alguma alternativa! 



##### VOTES AND SEATS DATASETS #####

# WEIGHTING SAMPLES TO REPRESENT VOTE SHARES: MULTIPLY BY SHARE OF VOTES / DIVIDE BY NUMBER OF INTERVIEWS

euro_cand$smp_votes <- round ((euro_cand$votes * 100) / euro_cand$interviews)
euro_sample <- completeFun(euro_cand, "smp_votes")

euro_votesmp <- euro_sample[rep(seq(nrow(euro_sample)), euro_sample$smp_votes),]

#SAME THING FOR VOTE SEATS NOW:

euro_cand$smp_seats <- round ((euro_cand$seats * 10) / euro_cand$interviews)
euro_seats <- completeFun(euro_cand, "smp_seats")

euro_seats_smp <- euro_seats[rep(seq(nrow(euro_seats)), euro_seats$smp_seats),]




##### SAME-SEX #####

# v020_03: Same-sex marriages should be prohibited by law Q58.


#DEIXAR SÓ VARIÁVEL DE INTERESSE, TIRAR MISSING ROWS NELA E CRIAR IDENTIFICADOR PARA CADA GRUPO A COMPARAR

#CITIZEN
cit_same_sex<- citizen %>% select (country, q58) %>% dplyr::rename (samps = q58) 

cit_same_sex <- completeFun(cit_same_sex, "samps")

cit_same_sex$pop <-  as.factor("x")

#CANDIDATES
cand_same_sex <- candidates %>% select (country, v020_03) %>% dplyr::rename (samps = v020_03) 

cand_same_sex <- completeFun(cand_same_sex, "samps")

cand_same_sex$pop <-  as.factor("y")


#BIND

bind_same_sex <- rbind(cit_same_sex, cand_same_sex)


#EMD - TOTAL CONGRUENCE
emd_smsex <- ddply(bind_same_sex, .(country), emd.dis)



##### CITIZEN-VOTER ######

#BANCO PARA voters e non-voters

abst_smsex <- citizen %>% filter (q24==2) %>% select (country, q58) %>% dplyr::rename (samps = q58) 

abst_smsex <- completeFun(abst_smsex, "samps")

abst_smsex$pop <-  as.factor("x")


voter_same_sex <- citizen %>% filter (q24==1) %>% select (country, q58) %>% dplyr::rename (samps = q58) 

voter_same_sex <- completeFun(voter_same_sex, "samps")

voter_same_sex$pop <-  as.factor("y")

bind_abv_same_sex <- rbind(abst_smsex, voter_same_sex)

emd_abv_same_sex <- ddply(bind_abv_same_sex, .(country), emd.dis)


##### VOTER-VOTES #####


euro_votesmp$pop <- as.factor("x")


votesmp_same_sex <- euro_votesmp %>% select (country, v020_03, pop) %>% dplyr::rename (samps = v020_03) 

votesmp_same_sex <- completeFun(votesmp_same_sex, "samps")

bind_voter_vts_same_sex <- rbind(voter_same_sex, votesmp_same_sex)

emd_vtr_vts_same_sex <- ddply(bind_voter_vts_same_sex, .(country), emd.dis)
 

 #####VOTES-SEATS#####

euro_seats_smp$pop <- as.factor("y")


seats_smp_same_sex <- euro_seats_smp %>% select (country, v020_03, pop) %>% dplyr::rename (samps = v020_03) 

seats_smp_same_sex <- completeFun(seats_smp_same_sex, "samps")

bind_vts_seats_same_sex <- rbind(votesmp_same_sex, seats_smp_same_sex)

emd_vts_seats_same_sex <- ddply(bind_vts_seats_same_sex, .(country), emd.dis)


#CANDIDATES-VOTER/SEATS/CITIZEN?





#The voting is compulsory in the following countries: Belgium, Bulgaria, Cyprus, Greece and Luxembourg.


