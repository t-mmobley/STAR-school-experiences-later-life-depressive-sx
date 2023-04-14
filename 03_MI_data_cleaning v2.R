# Packages ------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "dplyr", 
       "survey", "tidyr", "jtools", "GGally", "VGAM", "huxtable", "mclust",
       "summarytools", "ggstance", "multcomp", "cowplot", "epiDisplay",
       "openxlsx", "ggpubr", "TraMineR", "cluster", "readxl","gridGraphics",
       "TraMineRextras", "mice", "mitools", "rlang")

# Read -------------------------------------------------------------------
dat_pmm_stacked <- readRDS(paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/",
                                  "Data/dat_pmm_stacked_v2.RDS"))

# clean school data -----------------------------------------------------

# School attendance: If w1_school_att = 0 then change school vars to 9
grade1.vars<-c("w1_school_seg_1", "w1_school_reg_1", "w2_school_cared_1", 
               "w2_students_clean_1")
grade6.vars<-c("w1_school_seg_6", "w1_school_reg_6", "w2_school_cared_6", 
               "w2_students_clean_6")
grade9.vars<-c("w1_school_seg_9", "w1_school_reg_9", "w2_school_cared_9", 
               "w2_students_clean_9")
grade12.vars<-c("w1_school_seg_12", "w1_school_reg_12", "w2_school_cared_12", 
                "w2_students_clean_12")

for (i in grade1.vars){
  dat_pmm_stacked[,i]<-ifelse(dat_pmm_stacked$school_att_clean_1==0,9,
                              dat_pmm_stacked[,i])}
for (i in grade6.vars){
  dat_pmm_stacked[,i]<-ifelse(dat_pmm_stacked$school_att_clean_6==0,9,
                              dat_pmm_stacked[,i])}
for (i in grade9.vars){
  dat_pmm_stacked[,i]<-ifelse(dat_pmm_stacked$school_att_clean_9==0,9,
                              dat_pmm_stacked[,i])}
for (i in grade12.vars){
  dat_pmm_stacked[,i]<-ifelse(dat_pmm_stacked$school_att_clean_12==0,9,
                              dat_pmm_stacked[,i])}

# by school question across grades
att.vars<-c("school_att_clean_1", "school_att_clean_6", "school_att_clean_9", 
            "school_att_clean_12")
seg.vars<-c("w1_school_seg_1", "w1_school_seg_6", "w1_school_seg_9", 
            "w1_school_seg_12")
reg.vars<-c("w1_school_reg_1", "w1_school_reg_6", "w1_school_reg_9", 
            "w1_school_reg_12")
cared.vars<-c("w2_school_cared_1", "w2_school_cared_6", "w2_school_cared_9", 
              "w2_school_cared_12")
comp.vars<-c("w2_students_clean_1", "w2_students_clean_6", 
             "w2_students_clean_9", "w2_students_clean_12")

# school attendance 
for (i in att.vars){
  t <- table(eval(parse_expr(paste0("dat_pmm_stacked$",i))), exclude=NULL)
  print(t) # 1 (attended), 0 (did not attend)
  # dat_pmm_stacked[,i]<- ifelse(dat_pmm_stacked[,i]==1,"attend","notattend")
}

# segregation
for (i in seg.vars){
  t <- table(eval(parse_expr(paste0("dat_pmm_stacked$",i))), exclude=NULL)
  print(t) # 1 (no), 2 (yes), 9 (Did not attend) 
  
  dat_pmm_stacked[,i]<- ifelse(dat_pmm_stacked[,i]==2,1,
                               ifelse(dat_pmm_stacked[,i]==1,0,9))
  
  c <- table(eval(parse_expr(paste0("dat_pmm_stacked$",i))), exclude=NULL)
  print(c) # 0 (no), 1 (yes), 9 (Did not attend) 
  }

# school region
for (i in reg.vars){
  t <- table(eval(parse_expr(paste0("dat_pmm_stacked$",i))), exclude=NULL)
  print(t) # 1 (West), 2 (Midwest), 3 (South), 4 (Northeast), 9 (Did not attend) 
}

# having an adult/teacher who cared -- setting 9 (not attend) to not cared
for (i in cared.vars){
  t <- table(eval(parse_expr(paste0("dat_pmm_stacked$",i))), exclude=NULL)
  print(t) # 1 (not cared), 2 (cared), 9 (Did not attend) 
  dat_pmm_stacked[,i]<- as.factor(ifelse(dat_pmm_stacked[,i]==2,1,
                               ifelse(dat_pmm_stacked[,i] %in% c(1,9),0,NA)))
  c <- table(eval(parse_expr(paste0("dat_pmm_stacked$",i))), exclude=NULL)
  print(c) # 0 (not cared/did not attend), 1 (cared) 
  }

# school student composition
for (i in comp.vars){
  t <- table(eval(parse_expr(paste0("dat_pmm_stacked$",i))), exclude=NULL)
  print(t) # 1 (Black), 2 (white), 3 (Latino), 4 (mix), 5 (other), 9 (no attend) 
}

# create southern vs not southern and mostly Black students vs not
dat_pmm_stacked$w1_school_south_1<- 
  as.factor(ifelse(dat_pmm_stacked$w1_school_reg_1==3,1,0))
dat_pmm_stacked$w1_school_south_6<- 
  as.factor(ifelse(dat_pmm_stacked$w1_school_reg_6==3,1,0))
dat_pmm_stacked$w1_school_south_9<- 
  as.factor(ifelse(dat_pmm_stacked$w1_school_reg_9==3,1,0))
dat_pmm_stacked$w1_school_south_12<- 
  as.factor(ifelse(dat_pmm_stacked$w1_school_reg_12==3,1,0))

dat_pmm_stacked$w2_students_black_1<- 
  as.factor(ifelse(dat_pmm_stacked$w2_students_clean_1==1,1,0))
dat_pmm_stacked$w2_students_black_6<- 
  as.factor(ifelse(dat_pmm_stacked$w2_students_clean_6==1,1,0))
dat_pmm_stacked$w2_students_black_9<- 
  as.factor(ifelse(dat_pmm_stacked$w2_students_clean_9==1,1,0))
dat_pmm_stacked$w2_students_black_12<- 
  as.factor(ifelse(dat_pmm_stacked$w2_students_clean_12==1,1,0))

# student racial composition, 3-level
dat_pmm_stacked$w2_students_3cat_1<- 
  as.factor(ifelse(dat_pmm_stacked$w2_students_clean_1==1,1,
                   ifelse(dat_pmm_stacked$w2_students_clean_1==2,2,
                          ifelse(dat_pmm_stacked$w2_students_clean_1==4,3,NA))))
dat_pmm_stacked$w2_students_3cat_6<- 
  as.factor(ifelse(dat_pmm_stacked$w2_students_clean_6==1,1,
                   ifelse(dat_pmm_stacked$w2_students_clean_6==2,2,
                          ifelse(dat_pmm_stacked$w2_students_clean_6==4,3,NA))))
dat_pmm_stacked$w2_students_3cat_9<- 
  as.factor(ifelse(dat_pmm_stacked$w2_students_clean_9==1,1,
                   ifelse(dat_pmm_stacked$w2_students_clean_9==2,2,
                          ifelse(dat_pmm_stacked$w2_students_clean_9==4,3,NA))))
dat_pmm_stacked$w2_students_3cat_12<- 
  as.factor(ifelse(dat_pmm_stacked$w2_students_clean_12==1,1,
                   ifelse(dat_pmm_stacked$w2_students_clean_12==2,2,
                          ifelse(dat_pmm_stacked$w2_students_clean_12==4,3,NA))))
# checks
# table(dat_pmm_stacked$w2_students_3cat_1,
#       dat_pmm_stacked$w2_students_clean_1, exclude = NULL)
# table(dat_pmm_stacked$w2_students_3cat_6,
#       dat_pmm_stacked$w2_students_clean_6, exclude = NULL)
# table(dat_pmm_stacked$w2_students_3cat_9,
#       dat_pmm_stacked$w2_students_clean_9, exclude = NULL)
# table(dat_pmm_stacked$w2_students_3cat_12,
#       dat_pmm_stacked$w2_students_clean_12, exclude = NULL)
# table(dat_pmm_stacked$w1_school_reg_1,
#       dat_pmm_stacked$w1_school_south_1, exclude=NULL)
# table(dat_pmm_stacked$w1_school_seg_1, exclude=NULL)
# table(dat_pmm_stacked$w2_school_cared_1, exclude=NULL)
# table(dat_pmm_stacked$w2_students_clean_1,
#       dat_pmm_stacked$w2_students_black_1, exclude=NULL)

# Pivot longer -----------------------------------------------------

# first do age
age_long <- dat_pmm_stacked %>%
  dplyr::select(c("studyid","imp", "w1_age_s", "w2_age_s")) %>%
  arrange(studyid, imp) %>%
  group_by(studyid, imp) %>%
  pivot_longer(cols = contains("age"),
               names_to = "analysis_wave", values_to = "agewav_s") %>%
  mutate(analysis_wave = substr(analysis_wave, 1, 2),
         agewav65_s = agewav_s- 65)

# depression t scores
tscore_cont_long <- dat_pmm_stacked %>%
  dplyr::select(c("studyid","imp", "w1_tscore_s", "w2_tscore_s")) %>%
  arrange(studyid, imp) %>%
  group_by(studyid, imp) %>%
  pivot_longer(cols = contains("tscore"),
               names_to = "analysis_wave", values_to = "deprsx_tscore_s") %>%
  mutate(analysis_wave = substr(analysis_wave, 1, 2),
         deprsx_tscore_s = as.numeric(as.character(deprsx_tscore_s)))

# combine depr sx by id, imp, wave  
deprsx_long <- tscore_cont_long %>%
  left_join(age_long, by=c("studyid", "imp", "analysis_wave"))

# left join analytic data set
analysis_deprsx_long <- deprsx_long %>%
  left_join(dat_pmm_stacked, by=c('studyid', 'imp')) %>%
  dplyr::select(-c("w1_tscore_s", "w2_tscore_s"))

# create deprsx cc data set
analysis_deprsx_long <- analysis_deprsx_long %>%
  filter(!is.na(deprsx_tscore_s)) # TMM changing to tscore (was zscore var)

# check number of people (should be 739)
length(unique(analysis_deprsx_long$studyid)) # 739
# analysis_deprsx_long %>% filter(is.na(deprsx_tscore_s)) %>% count() # 0

# format tscores
analysis_deprsx_long$deprsx_tscore1sd_s <- 
  (analysis_deprsx_long$deprsx_tscore_s - 50)/10

# other covariates -------------------------------------------------

# partnered (including married)
# table(analysis_deprsx_long$maritalstatus, exclude=NULL)

analysis_deprsx_long$partnered <- 
  ifelse(analysis_deprsx_long$maritalstatus %in% c(1, 2), 1, 0)

analysis_deprsx_long$marstat_collapse <- 
  ifelse(analysis_deprsx_long$maritalstatus %in% c(1, 2), 1, # married/partnered
         ifelse(analysis_deprsx_long$maritalstatus %in% c(3, 4), 2, # separated/divorced
                ifelse(analysis_deprsx_long$maritalstatus == 5, 3, # widowed
                       ifelse(analysis_deprsx_long$maritalstatus == 6, # never married
                              4, NA))))
# table(analysis_deprsx_long$partnered, exclude=NULL)
# table(analysis_deprsx_long$marstat_collapse, exclude=NULL)

# retired (endorsing any indicators)

# check <- analysis_deprsx_long %>%
#   mutate(retire_fulltime = as.numeric(as.character(retire_fulltime)),
#          retire_parttime = as.numeric(as.character(retire_parttime)),
#          retire_notworking = as.numeric(as.character(retire_notworking)),
#     sum = retire_fulltime + retire_parttime + retire_notworking) %>%
#   filter(sum > 1)

analysis_deprsx_long$retired <- 
  ifelse(analysis_deprsx_long$retire_fulltime==1 | 
           analysis_deprsx_long$retire_parttime==1 |
           analysis_deprsx_long$retire_notworking==1,1,0)
# table(analysis_deprsx_long$retired)

# binary parental edu cutoffs -- from Chloe's KHANDLE poster, do > 8th grade vs <=

# summary(analysis_deprsx_long$maternal_edu) # range 0-20
# summary(analysis_deprsx_long$paternal_edu) # range 0-20
analysis_deprsx_long$maternal_edu8 <-
  ifelse(analysis_deprsx_long$maternal_edu > 8,1,0)
analysis_deprsx_long$paternal_edu8 <-
  ifelse(analysis_deprsx_long$paternal_edu > 8,1,0)

# table(analysis_deprsx_long$maternal_edu8, analysis_deprsx_long$maternal_edu, 
#       exclude=NULL)
# table(analysis_deprsx_long$paternal_edu8, analysis_deprsx_long$paternal_edu, 
#       exclude=NULL)

# fair/poor health (4 = fair and 5 = poor)
# table(analysis_deprsx_long$sr_health, exclude = NULL)
analysis_deprsx_long$fairpoor_health <-
  ifelse(analysis_deprsx_long$sr_health %in% c(4,5),1,0)
# table(analysis_deprsx_long$sr_health, analysis_deprsx_long$fairpoor_health)

# smoke status
# table(analysis_deprsx_long$smoke_status, exclude = NULL)
analysis_deprsx_long$ever_smoke <-
  ifelse(analysis_deprsx_long$smoke_status %in% c(1,2),1,0)
# table(analysis_deprsx_long$ever_smoke, exclude = NULL)

# concerned thinking
# table(analysis_deprsx_long$concerned_thinking, exclude = NULL)
# pain
# table(analysis_deprsx_long$pain, exclude = NULL)
# confidante
# table(analysis_deprsx_long$confidante, exclude = NULL)

# fair/poor hearing
# table(analysis_deprsx_long$hearing, exclude = NULL)
analysis_deprsx_long$fairpoor_hearing <-
  ifelse(analysis_deprsx_long$hearing %in% c(4,5),1,0)
# table(analysis_deprsx_long$fairpoor_hearing, exclude = NULL)

# fair/poor vision or legally blind
# table(analysis_deprsx_long$vision, exclude = NULL)
analysis_deprsx_long$fairpoor_vision <-
  ifelse(analysis_deprsx_long$vision %in% c(4,5,6),1,0)
# table(analysis_deprsx_long$fairpoor_vision, exclude = NULL)

# save dataframe ---------------------------------------------------
saveRDS(analysis_deprsx_long, 
     file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Data/",
                   "analysis_dat_v2.RDS"))
