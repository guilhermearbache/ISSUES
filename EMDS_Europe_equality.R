##### EQUALITY OF INCOME #####


# v020_08: Income and wealth should be redistributed towards ordinary people  ROES 4 Q63.


#DEIXAR SÓ VARIÁVEL DE INTERESSE, TIRAR MISSING ROWS NELA E CRIAR IDENTIFICADOR PARA CADA GRUPO A COMPARAR

#CITIZEN
cit_equality<- citizen %>% select (country, q63) %>% dplyr::rename (samps = q63) 

cit_equality <- completeFun(cit_equality, "samps")

cit_equality$pop <-  as.factor("x")

#CANDIDATES
cand_equality <- candidates %>% select (country, v020_08) %>% dplyr::rename (samps = v020_08) 

cand_equality <- completeFun(cand_equality, "samps")

cand_equality$pop <-  as.factor("y")


#BIND

bind_equality <- rbind(cit_equality, cand_equality)


#EMD - TOTAL CONGRUENCE
emd_equality <- ddply(bind_equality, .(country), emd.dis)



##### CITIZEN-VOTER ######

#BANCO PARA voters e non-voters

abst_equality <- citizen %>% filter (q24==2) %>% select (country, q63) %>% dplyr::rename (samps = q63) 

abst_equality <- completeFun(abst_equality, "samps")

abst_equality$pop <-  as.factor("x")


voter_equality <- citizen %>% filter (q24==1) %>% select (country, q63) %>% dplyr::rename (samps = q63) 

voter_equality <- completeFun(voter_equality, "samps")

voter_equality$pop <-  as.factor("y")

bind_abv_equality <- rbind(abst_equality, voter_equality)

emd_abv_equality <- ddply(bind_abv_equality, .(country), emd.dis)


##### VOTER-VOTES #####


euro_votesmp$pop <- as.factor("x")


votesmp_equality <- euro_votesmp %>% select (country, v020_08, pop) %>% dplyr::rename (samps = v020_08) 

votesmp_equality <- completeFun(votesmp_equality, "samps")

bind_voter_vts_equality <- rbind(voter_equality, votesmp_equality)

emd_vtr_vts_equality <- ddply(bind_voter_vts_equality, .(country), emd.dis)


#####VOTES-SEATS#####

euro_seats_smp$pop <- as.factor("y")


seats_smp_equality <- euro_seats_smp %>% select (country, v020_08, pop) %>% dplyr::rename (samps = v020_08) 

seats_smp_equality <- completeFun(seats_smp_equality, "samps")

bind_vts_seats_equality <- rbind(votesmp_equality, seats_smp_equality)

emd_vts_seats_equality <- ddply(bind_vts_seats_equality, .(country), emd.dis)