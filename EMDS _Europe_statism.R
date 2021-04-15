
##### STATE OWNERSHIP #####


#v020_04: Major public services and industries ought to be in state ownership (20.4) ROES 1  Q59.


#DEIXAR SÓ VARIÁVEL DE INTERESSE, TIRAR MISSING ROWS NELA E CRIAR IDENTIFICADOR PARA CADA GRUPO A COMPARAR

#CITIZEN
cit_statism<- citizen %>% select (country, q59) %>% dplyr::rename (samps = q59) 

cit_statism <- completeFun(cit_statism, "samps")

cit_statism$pop <-  as.factor("x")

#CANDIDATES
cand_statism <- candidates %>% select (country, v020_04) %>% dplyr::rename (samps = v020_04) 

cand_statism <- completeFun(cand_statism, "samps")

cand_statism$pop <-  as.factor("y")


#BIND

bind_statism <- rbind(cit_statism, cand_statism)


#EMD - TOTAL CONGRUENCE
emd_statism <- ddply(bind_statism, .(country), emd.dis)



##### CITIZEN-VOTER ######

#BANCO PARA voters e non-voters

abst_statism <- citizen %>% filter (q24==2) %>% select (country, q59) %>% dplyr::rename (samps = q59) 

abst_statism <- completeFun(abst_statism, "samps")

abst_statism$pop <-  as.factor("x")


voter_statism <- citizen %>% filter (q24==1) %>% select (country, q59) %>% dplyr::rename (samps = q59) 

voter_statism <- completeFun(voter_statism, "samps")

voter_statism$pop <-  as.factor("y")

bind_abv_statism <- rbind(abst_statism, voter_statism)

emd_abv_statism <- ddply(bind_abv_statism, .(country), emd.dis)


##### VOTER-VOTES #####


euro_votesmp$pop <- as.factor("x")


votesmp_statism <- euro_votesmp %>% select (country, v020_04, pop) %>% dplyr::rename (samps = v020_04) 

votesmp_statism <- completeFun(votesmp_statism, "samps")

bind_voter_vts_statism <- rbind(voter_statism, votesmp_statism)

emd_vtr_vts_statism <- ddply(bind_voter_vts_statism, .(country), emd.dis)


#####VOTES-SEATS#####

euro_seats_smp$pop <- as.factor("y")


seats_smp_statism <- euro_seats_smp %>% select (country, v020_04, pop) %>% dplyr::rename (samps = v020_04) 

seats_smp_statism <- completeFun(seats_smp_statism, "samps")

bind_vts_seats_statism <- rbind(votesmp_statism, seats_smp_statism)

emd_vts_seats_statism <- ddply(bind_vts_seats_statism, .(country), emd.dis)