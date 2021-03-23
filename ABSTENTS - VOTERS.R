lapcitizen <- completeFun(lapfinal, "vb2")

lapvoter <- lapcitizen %>% 
  filter (vb2==1)


lap_abstent <- lapcitizen %>% 
  filter (vb2==2)


lap_abstent$pop <- as.factor("x")
lapvoter$pop <- as.factor("y")

#rbind subset + set

lap_absvoter <- rbind(lap_abstent, lapvoter)

#rodar tudo  

##### SAME-SEX MARRIAGE #####

df_citvot_smsex <- lap_absvoter

names(df_citvot_smsex)[names(df_citvot_smsex) == 'same_sex'] <- 'samps'

df_citvot_smsex <- completeFun(df_citvot_smsex, "samps")

#EMD
emd_cv_smsex <- ddply(df_citvot_smsex, .(cname), emd.dis)


##### ROES 1#### 

df_citvot_ros1 <- lap_absvoter

names(df_citvot_ros1)[names(df_citvot_ros1) == 'ROES1'] <- 'samps'

df_citvot_ros1 <- completeFun(df_citvot_ros1, "samps")

#EMD
emd_cv_ros1 <- ddply(df_citvot_ros1, .(cname), emd.dis)

##### ROES2#### 

df_citvot_ros2 <- lap_absvoter

names(df_citvot_ros2)[names(df_citvot_ros2) == 'ROES2'] <- 'samps'

df_citvot_ros2 <- completeFun(df_citvot_ros2, "samps")

#EMD
emd_cv_ros2 <- ddply(df_citvot_ros2, .(cname), emd.dis)

### ROES 3 ###: 
df_citvot_ros3 <- lap_absvoter

names(df_citvot_ros3)[names(df_citvot_ros3) == 'ROES3'] <- 'samps'

df_citvot_ros3 <- completeFun(df_citvot_ros3, "samps")

#EMD
emd_cv_ros3 <- ddply(df_citvot_ros3, .(cname), emd.dis)

## LEFT-RIGHT:

df_citvot_LR <- lap_absvoter

names(df_citvot_LR)[names(df_citvot_LR) == 'ideol_self'] <- 'samps'

df_citvot_LR <- completeFun(df_citvot_LR, "samps")

#EMD
emd_cv_LR <- ddply(df_citvot_LR, .(cname), emd.dis)

write.xlsx(list(emd_cv_smsex=emd_cv_smsex, emd_cv_ros1=emd_cv_ros1, emd_cv_ros2=emd_cv_ros2, emd_cv_ros3=emd_cv_ros3, 
                emd_cv_LR = emd_cv_LR), "Abstent_voter_congruence.xlsx")




##### POR ALGUM MOTIVO MISTERIOSO, PERU NÃO ESTÁ ENTRANDO NESSAS ANÁLISES. FAREI POR FORA



PER_absvt<- lap_absvoter %>% filter (cname =="PER")

select (ROES1, pop) 
CHL_lap_cong <- CHL_lapvoter %>% select (ROES1, pop)


CHL_congvotes <-rbind(CHL_smp_cong, CHL_lap_cong)

CHL_congvotes <- CHL_congvotes %>% dplyr::rename (samps = ROES1) 

CHL_congvotes$samps <- as.numeric(CHL_congvotes$samps)

CHL_congvotes <- completeFun(CHL_congvotes, "samps")


CHL_emd_votes_ROES1<- emd.dis(CHL_congvotes)
CHL_dmeans <-dmeans (CHL_congvotes)
