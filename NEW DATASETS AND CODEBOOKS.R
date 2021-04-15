##### PACKAGES #####

library(haven)


#### VOTER STUDY #####

euro_voter <- read_dta (paste("C:/Users/livia/OneDrive - usp.br/TESE/DATASETS/",
                              "ISSUES - BANCOS CERTOS/European Parliament - voter 2009/",
                              "ZA5055_v1-1-1_stata14.dta", sep=""))

euro_voter$country <- sjlabelled::as_label(euro_voter$t102) 

euro_voter$country <- gsub("The Netherlands", "Netherlands", euro_voter$country) 

euro_voter <- euro_voter %>% select (country, t100:t106, q101:q103, q24:q28, 
                                     q46,q47_p1:q47_p15, q56:q67)

country_names <- read.csv ("C:/Users/livia/Desktop/ISSUES/Candidate_names.csv")

euro_voter$country_abr <-  country_names$country_abr [match(euro_voter$country,country_names$country)]


euro_voter <- euro_voter %>%  mutate_at(.vars = vars(q56:q67, q24), 
                                        .funs = funs(ifelse(. > 5, NA, .)))

##### CANDIDATE STUDY  #####

euro_cand <- read_dta ("C:/Users/livia/OneDrive - usp.br/TESE/DATASETS/ISSUES - BANCOS CERTOS/ZA5053_CandStudy2009_full/ZA5053_v2-0-0.dta")

euro_cand <- euro_cand %>%  mutate_at(.vars = vars(starts_with("v020"), starts_with("v018")), 
                                      .funs = funs(ifelse(. == -99, NA, .)))

euro_cand$country <-  country_names$country [match(euro_cand$country_abr,country_names$country_abr)]

# AGRUPAR
# 
# 


#Baixando planilha onde preenchi Votes and Seats para cada partido:
pty_seats <- read.csv("C:/Users/livia/Desktop/ISSUES/euro_pty_seats.csv", dec=",") 


euro_cand$votes <- pty_seats$votes [match(paste(euro_cand$country_abr, euro_cand$ptyname_eng),
paste(pty_seats$country_abr, pty_seats$ptyname_eng))]

euro_cand$seats <- pty_seats$seats [match(paste(euro_cand$country_abr, euro_cand$ptyname_eng),
                                          paste(pty_seats$country_abr, pty_seats$ptyname_eng))]

euro_cand$lsq <- pty_seats$LSQ [match(paste(euro_cand$country_abr, euro_cand$ptyname_eng),
                                          paste(pty_seats$country_abr, pty_seats$ptyname_eng))]


euro_cand <- euro_cand %>% relocate (country_abr, ptyname_eng, votes, seats, lsq)



#ALGUMAS INSERÇÕES QUE NÃO FUNCIONARAM PELO MATCH: 

euro_cand$votes[euro_cand$ptyname_eng == "People's Movement against the EU"] <- 7.2

euro_cand$seats[euro_cand$ptyname_eng == "People's Movement against the EU"] <- 1

euro_cand$lsq[euro_cand$ptyname_eng == "People's Movement against the EU"] <- 8.490099


euro_cand$votes[euro_cand$ptyname_eng == "Europe Écologie" & euro_cand$country_abr == "FR"] <- 16.28

euro_cand$seats[euro_cand$ptyname_eng == "Europe Écologie" & euro_cand$country_abr == "FR"] <- 14

euro_cand$lsq[euro_cand$ptyname_eng == "Europe Écologie" & euro_cand$country_abr == "FR"] <- 10.16545


#### NÚMERO DE ENTREVISTADOS POR PARTIDO #####

cand_grouped <- euro_cand %>%
  dplyr::group_by(country_abr, ptyname_eng) %>% 
  dplyr::summarise(n = n())

euro_cand$interviews <- cand_grouped$n [match(paste(euro_cand$country_abr, euro_cand$ptyname_eng),
                                              paste(cand_grouped$country_abr, cand_grouped$ptyname_eng))]


##### CÓDIGOS CENTRAIS #####


# v020_03: Same-sex marriages should be prohibited by law Q58.

#v020_04: Major public services and industries ought to be in state ownership (20.4) ROES 1  Q59.

# v020_08: Income and wealth should be redistributed towards ordinary people  ROES 4 Q63.

#v018_01: In terms of left and right, what is your political position? (18.1)

#Q46


# v020_05: Women should be free to decide on matters of abortion(20.5) Q60.
#v020_02: Private enterprise is the best way to solve [COUNTRY’S] economic problems Q57.




#mep
# 
# weight_pty
# weight_mep

#Artigo
replication <- read_spss (url("https://journals.sagepub.com/doi/suppl/10.1177/1465116516689729/suppl_file/eup-16-1189-File005_1.sav"))


##### DISPROPORTIONALITY


### LATAM - PEGAR DOS QUE TENHO MESMO - ALGUNS NÃO ESTÃO NA PLANILHA, PEGAR AQUI:

#https://www.tcd.ie/Political_Science/people/michael_gallagher/ElSystems/Docts/ElectionIndices.pdf
# http://christophergandrud.github.io/Disproportionality_Data/


#OU AQUI

carey <- read_dta ("C:/Users/livia/Downloads/Carey_Hix_AJPS_2011_data.dta")


# EUROPA - CALCULAR:

#https://www.tcd.ie/Political_Science/people/michael_gallagher/ElSystems/index.php - PARECE JÁ TER A FORMULA

# SE NÃO TIVER, TENTAR POR AQUI:
# https://www.youtube.com/watch?v=TyzFyqRckSY


# http://europeanelectionstudies.net/wp-content/uploads/2012/06/codebook-contextual.pdf - CONTEXTUAL DATA 
#(NÃO CONSEGUI ACHAR PARA BAIXAR, SÓ O CODEBOOK!!)



##### CODEBOOKS ##### 
## CANDIDATE STUDY ## 


# RETIRAR OS COM MAIS RESPOSTAS INVÁLIDAS? include


# Str. Agree = 1
# str. Disagree = 5
# No valid answer = -99





##### ISSUES #####
#v020_01: Immigrants should be required to adapt to the customs of [COUNTRY]

#v020_02: Private enterprise is the best way to solve [COUNTRY’S] economic problems

# v020_03: Same-sex marriages should be prohibited by law 


#v020_04: Major public services and industries ought to be in state ownership (20.4)

# v020_05: Women should be free to decide on matters of abortion(20.5)

#v020_06: Politics should abstain from intervening in the economy

# v020_07: People who break the law should be given much harsher sentences
 
# v020_08: Income and wealth should be redistributed towards ordinary people

# v020_09: Schools must teach children to obey authority

# v020_10: EU treaty changes should be decided by referendum

# v020_11: A woman should be prepared to cut down on her paid work for her family
 
# v020_12: Immigration to [COUNTRY] should be decreased significantly



##### OUTROS #####
 
# v029_1: European unification: candidate’s position

# v018_03: In terms of left and right, what is your party’s voters political position?

# ptyname: Party name (as used in candidate study)

# ptyname_special: Party name (as used in candidate study), details

# ptyfamily: Party family


# mep: Was the candidate elected



