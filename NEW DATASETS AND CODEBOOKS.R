library(haven)

euro_cand <- read_dta ("C:/Users/livia/OneDrive - usp.br/TESE/DATASETS/ISSUES - BANCOS CERTOS/ZA5053_CandStudy2009_full/ZA5053_v2-0-0.dta")

euro_voter <- read_dta (paste("C:/Users/livia/OneDrive - usp.br/TESE/DATASETS/",
        "ISSUES - BANCOS CERTOS/European Parliament - voter 2009/",
        "ZA5055_v1-1-1_stata14.dta", sep=""))

euro_voter$cname <- sjlabelled::as_label(euro_voter$t102) 

#Artigo
replication <- read_spss (url("https://journals.sagepub.com/doi/suppl/10.1177/1465116516689729/suppl_file/eup-16-1189-File005_1.sav"))



## CANDIDATE STUDY ## 

# Str. Agree = 1
# str. Disagree = 5
# No valid answer = -99


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


 
# v029_1: European unification: candidate’s position

# v018_03: In terms of left and right, what is your party’s voters political position?

# ptyname: Party name (as used in candidate study)

# ptyname_special: Party name (as used in candidate study), details

# ptyfamily: Party family


# mep: Was the candidate elected





#### VOTER STUDY #####

#  7 8 MISSING 
# 1 STR. AGREE 5 STR. DISAGREE 

eu_voter <- euro_voter %>% select (t100:t106, q101:q103, q24:q28, 
                                   q46,q47_p1:q47_p15, q56:q67)

get_label(eu_voter)

# Q101. What is the highest level of education you have completed in your education?
# 71 ‐ [OTHER, SPECIFY]: ____________________________
# 77 ‐ [REFUSED]
# 88 ‐ [DK]

# Q102. 1= MALE 2= FEMALE 7= MISSING 
#Q103 YEAR BORN 7777 ‐ [REFUSED]

#Q24 - q28 - voted, who voted for, would vote 

#Q46 - Self placement: left (0) right (10) 77 e 88 missing
#Q47 - Party placement 

# Q56. Immigrants should be required to adapt to the customs of 
# Q57. Private enterprise is the best way to solve Britain’s economic problems 
# Q58. Same‐sex marriages should be prohibited by law. 
# Q59. Major public services and industries ought to be in state ownership.. 
# Q60. Women should be free to decide on matters of abortion 
# Q61. Politics should abstain from intervening in the economy 
# Q62. People who break the law should be given much harsher sentences than
# they are these days.

# Q63. Income and wealth should be redistributed towards ordinary people
# 
# Q64. Schools must teach children to obey authority. 
# Q65. EU treaty changes should be decided by referendum 
# Q66. A woman should be prepared to cut down on her paid work for the sake of her family.
# 
# Q67. Immigration to Britain should be decreased significantly.




