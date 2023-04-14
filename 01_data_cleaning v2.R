# Loading packages -----------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "dplyr", 
       "survey", "tidyr", "jtools", "GGally", "VGAM", "huxtable",
       "summarytools", "ggstance", "multcomp", "cowplot", "epiDisplay",
       "openxlsx", "ggpubr")

# Read -----------------------------------------------------------------
# Crystal's path
# dat <- read_sas(paste0("/Users/crystalshaw/Library/CloudStorage/Box-Box/", 
#                        "STAR_coded_data/Raw_data_tables/STAR_AllWave_data/", 
#                        "STAR_AllWave_data_20211217/",
#                        "star_all_waves_20211217.sas7bdat"))

dat <- read_sas(paste0("C:/Users/tmobley/Box/STAR_coded_data/Raw_data_tables/",
                       "STAR_AllWave_data/STAR_AllWave_data_20220310/",
                       "star_all_waves_20220309.sas7bdat"))

colnames(dat) <- tolower(colnames(dat))

dat_covars <- dat %>% 
  dplyr::select(studyid, w1_interview_age, w2_interview_age, 
                w1_d_gender, w1_d_race_summary,
                w1_maternal_education, w1_maternal_education_text,
                w1_paternal_education_text, w1_paternal_education, 
                w1_income_range, w1_marital_status, w1_health, 
                w1_emp_notworking, w1_emp_fulltime, 
                w1_emp_parttime, w1_smk, w1_smk_now, w1_country_born,
                w1_res_1_geocode_state, w1_ecog_concerned_thinking,
                w1_confidante, w1_pain, w1_sensimp_vision, w1_sensimp_hearing)

# w1 covariate cleaning -------------------------------------------
dat_clean <- data.frame(studyid=dat_covars$studyid)

# age (continuous, top-coded at 90)
dat_clean$w1_age <- dat_covars$w1_interview_age 
# summary(dat_clean$w1_age)
dat_clean$w2_age <- dat_covars$w2_interview_age 

# sex
# table(dat_covars$w1_d_gender, exclude=NULL)
dat_clean$female<-ifelse(dat_covars$w1_d_gender==2,1,
                         ifelse(dat_covars$w1_d_gender==1,0,NA))

# table(dat_clean$female, dat_covars$w1_d_gender, exclude=NULL)

# race and ethnicity
# table(dat_covars$w1_d_race_summary, exclude = NULL)
dat_clean$raceth<- 
  ifelse(dat_covars$w1_d_race_summary=='Black',1,
         ifelse(dat_covars$w1_d_race_summary=='LatinX',2,
                ifelse(dat_covars$w1_d_race_summary=='Native American',3,NA)))
# table(dat_covars$w1_d_race_summary, dat_clean$raceth, exclude=NULL)

# income (categorical)
dat_clean$income <- 
  ifelse(dat_covars$w1_income_range %in% c(88,99),NA,
      ifelse(!is.na(dat_covars$w1_income_range),dat_covars$w1_income_range,NA))
# table(dat_clean$income, dat_covars$w1_income_range, exclude=NULL)

# marital status
# table(dat_covars$w1_marital_status, exclude=NULL)
dat_clean$maritalstatus <- ifelse(dat_covars$w1_marital_status!=88,
                                  dat_covars$w1_marital_status,NA)
# table(dat_clean$maritalstatus, exclude=NULL)

# employment -- no missing (endorsed or didn't)
# table(dat_covars$w1_emp_fulltime, exclude=NULL)
# table(dat_covars$w1_emp_notworking, exclude=NULL)
# table(dat_covars$w1_emp_parttime, exclude=NULL)

dat_clean$retire_fulltime <- dat_covars$w1_emp_fulltime
dat_clean$retire_notworking <- dat_covars$w1_emp_notworking
dat_clean$retire_parttime <- dat_covars$w1_emp_parttime

# confidante -- 1 = yes, 2 = no
# table(dat_covars$w1_confidante, exclude = NULL)
dat_clean$confidante <- ifelse(dat_covars$w1_confidante==1,1,
                               ifelse(dat_covars$w1_confidante==2,0,NA))
# table(dat_clean$confidante, exclude = NULL)

# smoking status
# table(dat_covars$w1_smk, exclude=NULL)
# table(dat_covars$w1_smk_now, exclude=NULL)
dat_clean$smoke_status<-
  ifelse(dat_covars$w1_smk==1 & is.na(dat_covars$w1_smk_now),1,
         ifelse(dat_covars$w1_smk==1 & dat_covars$w1_smk_now==1,2,
                ifelse(dat_covars$w1_smk==1,1,
                       ifelse(dat_covars$w1_smk==0,0,NA))))

# table(dat_covars$w1_smk, dat_covars$w1_smk_now, exclude=NULL)
# table(dat_clean$smoke_status, exclude=NULL)

# pain -- 2 88s
# table(dat_covars$w1_pain, exclude=NULL)
dat_clean$pain <- ifelse(dat_covars$w1_pain!=88,
                                  dat_covars$w1_pain,NA)
# table(dat_clean$pain, exclude=NULL)

# concerned memory -- 1 = yes, 2 = no
# table(dat_covars$w1_ecog_concerned_thinking, exclude=NULL)
dat_clean$concerned_thinking <- 
  ifelse(dat_covars$w1_ecog_concerned_thinking==1,1,
         ifelse(dat_covars$w1_ecog_concerned_thinking==2,0,NA))
# table(dat_clean$concerned_thinking, exclude=NULL)

# vision -- 1 - 6 (legally blind) 
# table(dat_covars$w1_sensimp_vision, exclude=NULL)
dat_clean$vision <- 
  ifelse(dat_covars$w1_sensimp_vision %in% c(1,2,3,4,5,6),
         dat_covars$w1_sensimp_vision,NA)
# table(dat_clean$vision, exclude=NULL)

# hearing -- 1 - 5 (Likert) 
# table(dat_covars$w1_sensimp_hearing, exclude=NULL)
dat_clean$hearing <- 
  ifelse(dat_covars$w1_sensimp_hearing %in% c(1,2,3,4,5),
         dat_covars$w1_sensimp_hearing,NA)
# table(dat_clean$hearing, exclude=NULL)

# childhood ses: s birth, parental edu
# table(dat_covars$w1_res_1_geocode_state, exclude=NULL) # 71 blank
dat_clean$sbirth <- 
  as.factor(ifelse(dat_covars$w1_res_1_geocode_state %in% 
                     c("Delaware", "District of Columbia", "Florida",
                      "Georgia", "Maryland", "North Carolina",
                      "South Carolina", "Virginia", "West Virginia",
                      "Alabama", "Kentucky", "Mississippi",
                      "Tennessee", "Arkansas", "Louisiana",
                      "Oklahoma", "Texas"),1,
                   ifelse(dat_covars$w1_res_1_geocode_state=='',NA,0)))
# table(dat_clean$sbirth, exclude=NULL)

# born in us indicator 1/0 from country_born
# also saying if w1_res_1_geocode_state (state of birth) is not na, then usborn==1
# table(dat_covars$w1_country_born, exclude=NULL)
dat_clean$usborn<-ifelse(dat_covars$w1_country_born==1,1,
                         ifelse(dat_covars$w1_country_born==2,0,NA))
# table(dat_clean$usborn, dat_clean$sbirth, exclude=NULL)

# Note: including the is.na(dat_clean$usborn) portion does not change usborn responses
dat_clean$usborn <-
  ifelse(!is.na(dat_clean$sbirth) & is.na(dat_clean$usborn),1,dat_clean$usborn)
# table(dat_clean$usborn, dat_clean$sbirth, exclude=NULL) # now 4 missing

# using continuous education in years
dat_clean$maternal_edu <- 
  ifelse(!is.na(dat_covars$w1_maternal_education_text),
         dat_covars$w1_maternal_education_text,
    ifelse(dat_covars$w1_maternal_education==1,13,
      ifelse(dat_covars$w1_maternal_education==2,14,
         ifelse(dat_covars$w1_maternal_education==3,16,
            ifelse(dat_covars$w1_maternal_education==4,18,
               ifelse(dat_covars$w1_maternal_education==5,20,
                  ifelse(dat_covars$w1_maternal_education %in% 
                           c(66,88,99),0,NA))))))) 

dat_clean$paternal_edu <- 
  ifelse(!is.na(dat_covars$w1_paternal_education_text),
         dat_covars$w1_paternal_education_text,
    ifelse(dat_covars$w1_paternal_education==1,13,
      ifelse(dat_covars$w1_paternal_education==2,14,
        ifelse(dat_covars$w1_paternal_education==3,16,
          ifelse(dat_covars$w1_paternal_education==4,18,
            ifelse(dat_covars$w1_paternal_education==5,20,
              ifelse(dat_covars$w1_paternal_education %in% 
                       c(66,88,99),0,NA))))))) 

# sr health
# table(dat_covars$w1_health, exclude=NULL)

dat_clean$sr_health <- ifelse(dat_covars$w1_health==88,NA,dat_covars$w1_health)

dat_clean$fairpoor_health <- ifelse(dat_clean$sr_health %in% c(4,5),1,
                              ifelse(dat_clean$sr_health %in% c(1,2,3),0,NA))

# Add school data ------------------------------------------------------

school_dat <- dat %>% 
  dplyr::select("studyid", (contains("school_1") & !contains("sta")) | 
                  (contains("school_6") & !contains("sta")) | 
                  (contains("school_9") & !contains("sta")) |
                  (contains("school_12") & !contains("sta")) | 
                  (starts_with("w2_school") & 
                     (ends_with("1") | ends_with("1_text"))) |
                  (starts_with("w2_school") & 
                     (ends_with("6") | ends_with("6_text"))) |
                  (starts_with("w2_school") & 
                     (ends_with("9") | ends_with("9_text"))) |
                  (starts_with("w2_school") & 
                     (ends_with("12") | ends_with("12_text"))) |
                  contains("edu_edu")) %>%
  dplyr::select(-c(contains('w2_edu'))) %>%
  rename(
    w1_school_att_1 = w1_school_1_01_att,
    w1_school_att_6 = w1_school_6_01_att,
    w1_school_att_9 = w1_school_9_01_att,
    w1_school_att_12 = w1_school_12_01_att,
    w1_school_reg_1 = w1_d_school_1_region,
    w1_school_reg_6 = w1_d_school_6_region,
    w1_school_reg_9 = w1_d_school_9_region,
    w1_school_reg_12 = w1_d_school_12_region,
    w1_school_seg_1 = w1_school_1_13_seg,
    w1_school_seg_6 = w1_school_6_13_seg,
    w1_school_seg_9 = w1_school_9_13_seg,
    w1_school_seg_12 = w1_school_12_13_seg,
    w2_school_att_1 = w2_school_1_01_att,
    w2_school_att_6 = w2_school_6_01_att,
    w2_school_att_9 = w2_school_9_01_att,
    w2_school_att_12 = w2_school_12_01_att,
    w2_school_students_text_1 = w2_school_students_1_text,
    w2_school_students_text_6 = w2_school_students_6_text,
    w2_school_students_text_9 = w2_school_students_9_text,
    w2_school_students_text_12 = w2_school_students_12_text)

# Crystal helped me with this! :)
tall_data <- school_dat %>% 
  pivot_longer(cols = -c(studyid, w1_edu_education, w1_edu_education_text),
               names_to = c(".value", "grade"),
               names_pattern = "(.*)(_\\d+)") %>% 
  #cleaning up unnecessary underscores
  mutate_at(.vars = c("grade"), function(x) str_remove(x, "_"))

# school attendance 
# if either w1 or w2 == 1 then use that one, else take first not missing

# table(tall_data$w1_school_att, tall_data$w2_school_att,
#       tall_data$grade, exclude=NULL)

tall_data$w1_school_att <- 
  as.numeric(as.character(ifelse(tall_data$w1_school_att==88,NA,
                                 tall_data$w1_school_att)))

tall_data$w2_school_att <- 
  as.numeric(as.character(ifelse(tall_data$w2_school_att==88,NA,
                                 tall_data$w2_school_att)))

# table(tall_data$w1_school_att, tall_data$w2_school_att,
#       tall_data$grade, exclude=NULL)

tall_data$school_att_clean <- 
  as.numeric(as.character(ifelse(!is.na(tall_data$w1_school_att) & 
                                   tall_data$w1_school_att==1,1,
                                 ifelse(!is.na(tall_data$w2_school_att) &
                                          tall_data$w2_school_att==1,1,
                                 ifelse(!is.na(tall_data$w1_school_att) &
                                          tall_data$w1_school_att==0,0,
                                 tall_data$w2_school_att)))))

# TMM adding check to make sure people with school att == 0 or NA do not have
# other school data
# check <- tall_data %>% filter(school_att_clean %in% c(0,NA)) # all missing

# one person missing all attendance. They are also missing PROMIS scores
# so will be dropped

# table(tall_data$school_att_clean, tall_data$grade, exclude=NULL)
# check <- tall_data %>% filter(is.na(school_att_clean)) # studyid = 04726
# check <- dat %>% filter(studyid=='04726') %>% 
#   dplyr::select(c(contains("PROMIS"))) 

# Teacher/adult at school who cared

# table(tall_data$w2_school_cared, exclude=NULL)
tall_data$w2_school_cared <- 
  as.factor(ifelse(tall_data$w2_school_cared==1,1,
                   ifelse(tall_data$w2_school_cared==0,0,NA)))
# table(tall_data$w2_school_cared, exclude=NULL)

# School Segregation (w1)

# table(tall_data$w1_school_seg, exclude=NULL)
tall_data$w1_school_seg <- 
  as.factor(ifelse(tall_data$w1_school_seg==1,1,
                   ifelse(tall_data$w1_school_seg==0,0,NA)))
# table(tall_data$w1_school_seg, exclude=NULL)

# School census region (w1) -- missing values already NA

# previously set region to 5 if usborn = 0 and missing school region
# decided this probably is unnecessary and misclassifies people incorrectly in
# one direction, so keeping as is - TMM 3/28/2023

# table(tall_data$w1_school_reg, exclude=NULL)
tall_data$w1_school_reg <- as.factor(tall_data$w1_school_reg)

# School student body composition

# table(tall_data$w2_school_students,
#        tall_data$w2_school_students_text,exclude=NULL)
# table(tall_data$w2_school_students_text, exclude=NULL) # 3030 obs not free txt

tall_data$w2_students_clean <- 
  as.factor(ifelse(tall_data$w2_school_students %in% c(1,2,3,4),
                   tall_data$w2_school_students, 
    ifelse(tall_data$w2_school_students_text %in% c('ASIAN', 'CHINESE'),5,
      ifelse(tall_data$w2_school_students_text %in% 
               c('CLASSROOOM WAS MULTI-ETHNIC BUT SCHOOL WAS MOSTLY BLACK',
                 'MOSTLY BLACK W/A FEW HISPANICS',
                 'MIX BUT MOSTLY BLACK', 'HOME-SCHOOLED'),1,
        ifelse(tall_data$w2_school_students_text %in%
                 c('2 DIFFERENT SCHOOLS 1. MOSTLY WHITE, THEN 2) MIXED RACE'),2,
          ifelse(tall_data$w2_school_students_text %in% 
                   c('BLACK & LATINO','HALF BLACK, OTHER HALF A MIX', 
                     'JUST A FEW WHITES; MOSTLY BLACK & HISPANIC', 
                     'WHITE AND ASIAN','BLACK AND WHITE',
                     'MIX OF BLACK AND LATINO','WHITE/ SOMOAN/BLACK',
                     '2 SCHOOLS, 1ST WAS A MIX, SECOND WAS WHITE AND BLACK',
                     'HALF BLACK AND THE OTHER HALF WAS A MIX',
                     'BLACK & HISPANIC (NOT MANY WHITE',
                     'HISPANIC/LATINO AND WHITE', 'MOSTLY WHITE AND BLACK',
                     'MOSTLY WHITE AND HISPANIC','WHITE AND ASIAN',
                     'TWO SCHOOLS--ONE WAS A MIX, ONE MOSTLY WHITE. TRANSFERRED IN IDDLE OF 9TH GRADE',
                     'TWO SCHOOLS IN 9TH GRADE?FIRST WAS MIXED, SECOND MOSTLY WHITE'),
                 4,NA))))))

# table(tall_data$w2_students_clean, exclude=NULL)

# w1 educational attainment
# table(tall_data$w1_edu_education, tall_data$w1_edu_education_text,
#       exclude=NULL)
tall_data$w1_edu_yrs <- 
  ifelse(!is.na(tall_data$w1_edu_education_text),
         tall_data$w1_edu_education_text, #years 0-12 carry through
         ifelse(tall_data$w1_edu_education==1,13, #some college
                ifelse(tall_data$w1_edu_education==2,14, #associates
                       ifelse(tall_data$w1_edu_education==3,16, #bachelors
                              ifelse(tall_data$w1_edu_education==4,18,#masters
                                     ifelse(tall_data$w1_edu_education==5,20,
                                            NA)))))) #doctoral degree
# table(tall_data$w1_edu_yrs, tall_data$w1_edu_education_text, exclude=NULL)

# transform to wide data set
tall_data_clean <- tall_data %>% 
  dplyr::select(-c(w1_edu_education, w1_edu_education_text, 
                   w2_school_students, w2_school_students_text,
                   w1_school_att, w2_school_att)) # TMM removed usborn indicator 3/28/2023

wide_data <- tall_data_clean %>%
  pivot_wider(names_from = grade, names_sep="_",
              values_from=c(school_att_clean, w1_school_seg,
                            w1_school_reg, w2_school_cared, w2_students_clean))

# merge school data with clean dataframe
dat_school <- merge(x=dat_clean,y=wide_data, by='studyid', all=TRUE)

# PROMIS Depression scores,  short form v 8a  ------------------------
dat_depr <- dat %>% 
  dplyr::select("studyid", "w1_promis_10", "w1_promis_12", 
                "w1_promis_24","w1_promis_30", "w1_promis_19", "w1_promis_28",
                "w1_promis_11", "w1_promis_14",
                "w2_promis_10", "w2_promis_12", 
                "w2_promis_24","w2_promis_30", "w2_promis_19", "w2_promis_28",
                "w2_promis_11", "w2_promis_14")

# Step 1. Calculate sum of scores + number of items answered per participant
dat_depr[dat_depr == 88] <- NA

# summing this way only creates a raw_score for people who answered all 8 Qs
depr1 <- dat_depr %>% dplyr::select("studyid",starts_with("w1")) 
depr1 <- depr1 %>%
  mutate("raw_score" = depr1 %>%
           dplyr::select(contains("promis")) %>%
           apply(1, function(x) sum(x, na.rm=TRUE)),
         "w1_nitems" = depr1 %>%
           dplyr::select(contains("promis")) %>%
           apply(1, function(x) sum(1 - is.na(x)))) 

depr2 <- dat_depr %>% dplyr::select("studyid",starts_with("w2")) 
depr2 <- depr2 %>% 
  mutate("raw_score" = depr2 %>%
           dplyr::select(contains("promis")) %>%
           apply(1, function(x) sum(x, na.rm=TRUE)),
         "w2_nitems" = depr2 %>%
           dplyr::select(contains("promis")) %>%
           apply(1, function(x) sum(1 - is.na(x)))) 

# keep scores for people who answered all 8 items (previously used >= 6 items)
depr1$raw_score <-
  ifelse(depr1$w1_nitems==8,depr1$raw_score, NA)

depr2$raw_score <-
  ifelse(depr2$w2_nitems==8,depr2$raw_score, NA)

depr1 %>% group_by(w1_nitems) %>%
  summarize(min = min(raw_score),
            max = max(raw_score),
            mean = mean(raw_score))

depr2 %>% group_by(w2_nitems) %>%
  summarize(min = min(raw_score),
            max = max(raw_score),
            mean = mean(raw_score))

# check
# table(dat_depr$nitems)

# Step 2. Calculate T-scores
a <- 8:40
b <- c(38.2, 44.7, 47.5, 49.4, 50.9, 52.1, 53.2, 54.1, 55.1, 55.9, 56.8, 57.7,
       58.5, 59.4, 60.3, 61.2, 62.1, 63.0, 63.9, 64.9, 65.8, 66.8, 67.7, 68.7,
       69.7, 70.7, 71.7, 72.8, 73.9, 75.0, 76.4, 78.2, 81.3) 
c <- c(5.7, 3.3, 2.7, 2.3, 2.0, 1.9, 1.8, 1.8, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 
       1.7, 1.7, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 
       1.8, 1.9, 2.0, 2.4, 3.4) 

promis_depr8a <- tibble("raw_score" = a, "tscore" = b, "se" = c)

promis_depr8a$deprsx_modsev <- 
  ifelse(promis_depr8a$tscore >= 60.0,1,
                   ifelse(is.na(promis_depr8a$tscore),NA,0))

# merge and format w1 scores
depr1_clean = merge(x=depr1, y=promis_depr8a, by="raw_score", all.x=TRUE)
colnames(depr1_clean)[12:14] <- 
  paste("w1", colnames(depr1_clean[,c(12:14)]), sep = "_")
colnames(depr1_clean)[1] <- paste("w1_raw_score")

# merge and format w2 scores
depr2_clean = merge(x=depr2, y=promis_depr8a, by="raw_score", all.x=TRUE)
colnames(depr2_clean)[12:14] <- 
  paste("w2", colnames(depr2_clean[,c(12:14)]), sep = "_")
colnames(depr2_clean)[1] <- paste("w2_raw_score")

depr1_clean <- depr1_clean %>% 
  dplyr::select("studyid","w1_nitems", "w1_raw_score", "w1_tscore", "w1_se",
                "w1_deprsx_modsev")

depr2_clean <- depr2_clean %>% 
  dplyr::select("studyid", "w2_nitems", "w2_raw_score", "w2_tscore", "w2_se",
                "w2_deprsx_modsev")

depr_clean <- depr1_clean %>%
  left_join(depr2_clean)

# merge depression scores with clean data

dat_school_depr <- merge(x=dat_school,y=depr_clean, by='studyid', all=TRUE)

# check freqs for different depression category cutoffs
# table(dat_depr_clean$w1_deprsx_modsev, exclude=NULL) 

# analytic sample pre-MI ------------------------------------------

# check any 90+ ages - 11 people [TMM keeping these people]
# check <- dat_school_depr %>% filter(w1_age==89.9999 | w2_age==89.9999) # 11 obs
# length(unique(check$studyid)) # 11 

# 2. Keep people with at least one depression score (should get n = 731)

# check
# summary(dat_school_depr$w1_raw_score) # 39 NA's
# summary(dat_school_depr$w2_raw_score) # 382 NA's
# check <- dat_school_depr %>% filter(is.na(w2_tscore) & is.na(w1_tscore)) # 25

dat_school_depr <- dat_school_depr %>% 
  filter(!is.na(w1_raw_score) | !is.na(w2_raw_score))

# summary(dat_school_depr$w1_age) 
# check <- dat_school_depr %>% filter(is.na(w1_raw_score) & is.na(w2_raw_score))

# analysis baseline and follow-up depression scores -----------------

# baseline score [first wave if not missing, else second wave]
# table(dat_school_depr$w1_raw_score, exclude=NULL) # 14 people missing w1
dat_school_depr$w1_rawscore_s <- 
  ifelse(!is.na(dat_school_depr$w1_raw_score), dat_school_depr$w1_raw_score,
         dat_school_depr$w2_raw_score)

# follow up score: second wave score if not missing both first and second
# 368 people have both
# check <- dat_school_depr %>% filter(!is.na(w1_tscore) & !is.na(w2_tscore))

dat_school_depr$w2_rawscore_s <- 
  ifelse(!is.na(dat_school_depr$w1_raw_score) & 
           !is.na(dat_school_depr$w2_raw_score), 
         dat_school_depr$w2_raw_score, NA)

# check
# summary(dat_school_depr$w1_rawscore_s) # 8-38
# summary(dat_school_depr$w2_rawscore_s) # 8-26
# check <- dat_school_depr %>% filter(is.na(w1_rawscore_s) & is.na(w2_rawscore_s))

# t-scores
dat_school_depr$w1_tscore_s <- 
  ifelse(!is.na(dat_school_depr$w1_tscore), dat_school_depr$w1_tscore,
         dat_school_depr$w2_tscore)

dat_school_depr$w2_tscore_s <- 
  ifelse(!is.na(dat_school_depr$w1_tscore) & 
           !is.na(dat_school_depr$w2_tscore), 
         dat_school_depr$w2_tscore, NA)

# modsev deprsx modsev
dat_school_depr$w1_deprsx_modsev_s <- 
  ifelse(!is.na(dat_school_depr$w1_tscore), dat_school_depr$w1_deprsx_modsev,
         dat_school_depr$w2_deprsx_modsev)

dat_school_depr$w2_deprsx_modsev_s <- 
  ifelse(!is.na(dat_school_depr$w1_tscore) & 
           !is.na(dat_school_depr$w2_tscore), 
         dat_school_depr$w2_deprsx_modsev, NA)

# check
# summary(dat_school_depr$w1_tscore_s) # 38.2-76.4
# summary(dat_school_depr$w2_tscore_s) # 38.2-63.9

# sample baseline and follow up ages ----------------------------------
dat_school_depr$w1_age_s <- 
  ifelse(!is.na(dat_school_depr$w1_raw_score), dat_school_depr$w1_age,
         dat_school_depr$w2_age)

dat_school_depr$w2_age_s <- 
  ifelse(!is.na(dat_school_depr$w1_raw_score) & 
           !is.na(dat_school_depr$w2_raw_score), 
         dat_school_depr$w2_age,NA)

# summary(dat_school_depr$w1_age_s)
# summary(dat_school_depr$w2_age_s) # 371 missing
# depression z-score baseline sample ---------------------------------

# Note: these variables are not used in the paper, but leaving them here so  
# later scripts run. I'm planning to remove these variables after code review. 
mean <- mean(dat_school_depr$w1_rawscore_s) 
sd <- sd(dat_school_depr$w1_rawscore_s) 

dat_school_depr$w1_zscore_s <- 
  (dat_school_depr$w1_rawscore_s - mean)/sd
dat_school_depr$w2_zscore_s <- 
  (dat_school_depr$w2_rawscore_s - mean)/sd

dat_school_depr %>% summarize(w1mean = mean(w1_zscore_s, na.rm = TRUE),
                             w1sd = sd(w1_zscore_s, na.rm = TRUE),
                             w2mean = mean(w2_zscore_s, na.rm = TRUE),
                             w2sd = sd(w2_zscore_s, na.rm = TRUE))

# save dataframe 
saveRDS(dat_school_depr, 
        file=paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/", 
                    "Data/dat_school_depr.RDS")) 

# get student composition free text responses for suppl table 1
supp_tab <- tall_data %>% filter(w2_school_students_text != "" & 
                                   studyid %in% dat_school_depr$studyid) %>%
  mutate(grade = as.numeric(grade)) %>%
  arrange(w2_students_clean) %>%
  dplyr::select(c(studyid, grade, w2_students_clean, w2_school_students_text))

# length(unique(supp_tab$studyid)) #22 people and 26 obs

write.xlsx(supp_tab, 
           paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/results/",
                  "supp_tab1.xlsx"))
