# Loading packages, options and initializations ------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "tidyverse","ggplot2",
       "survey", "tableone", "mice", "openxlsx")

# Read -----------------------------------------------------------------
dat_school_depr <- 
  readRDS(paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Data/",
                 "dat_school_depr.RDS"))

# TMM changing list to exclude all outcome vars except analysis outcome vars
dat_imput <- dat_school_depr %>% 
  dplyr::select(-c(w1_tscore, w1_se, w1_zscore_s, 
                   w1_nitems, w2_tscore, w2_se, w2_zscore_s, 
                   w2_nitems, w2_age, w1_age, w2_age_s, 
                   w1_raw_score, w2_raw_score, 
                   w1_rawscore_s, w2_rawscore_s, 
                   w1_deprsx_modsev, w2_deprsx_modsev, w1_deprsx_modsev_s, 
                   w2_deprsx_modsev_s))

vars <- colnames(dat_imput)

# var check
# check sociodemographic var missingness 
# summary(dat_imput$w1_age_s) # TMM changing age to study baseline age
# summary(dat_imput$w1_tscore_s) # TMM checking w1 tscore
# summary(dat_imput$w2_tscore_s) # TMM checking w2 tscore

# table(dat_imput$female, exclude = NULL)
# table(dat_imput$sr_health, exclude = NULL)
# table(dat_imput$raceth, exclude = NULL)
# table(dat_imput$sbirth, exclude = NULL)
# table(dat_imput$income, exclude = NULL)
# table(dat_imput$maritalstatus, exclude = NULL)
# table(dat_imput$retire_fulltime, exclude = NULL)
# table(dat_imput$retire_parttime, exclude = NULL)
# table(dat_imput$retire_notworking, exclude = NULL)
# table(dat_imput$smoke_status, exclude = NULL)
# table(dat_imput$usborn, exclude = NULL)
# table(dat_imput$maternal_edu, exclude = NULL)
# table(dat_imput$paternal_edu, exclude = NULL)
# table(dat_imput$w1_edu_yrs, exclude = NULL)
# table(dat_imput$concerned_thinking, exclude=NULL)
# table(dat_imput$confidante, exclude=NULL)
# table(dat_imput$pain, exclude = NULL)
# table(dat_imput$vision, exclude = NULL)
# table(dat_imput$hearing, exclude = NULL)

# table(dat_imput$school_att_clean_1, exclude = NULL)
# table(dat_imput$school_att_clean_6, exclude = NULL)
# table(dat_imput$w1_school_seg_1, exclude = NULL)
# table(dat_imput$w1_school_reg_1, exclude = NULL)
# table(dat_imput$w2_school_cared_1, exclude = NULL)
# table(dat_imput$w2_students_clean_1, exclude = NULL)


# **Code adapted from EHL's KW multiple imputations script
# var list
# List of variables to impute/include in MICE
impute.var.list<- c("w1_age_s","w1_tscore_s","w2_tscore_s", # TMM adding deprsx
                    "female", "raceth", "sbirth", "income", 
                    "maritalstatus", "retire_fulltime", "retire_notworking",
                    "retire_parttime",  "w1_edu_yrs",
                    "smoke_status", "concerned_thinking",
                    "confidante", "pain", "vision", "hearing",
                    "usborn", "maternal_edu", "paternal_edu", "sr_health",  
                    "school_att_clean_1", "school_att_clean_6",
                    "school_att_clean_9", "school_att_clean_12",
                    "w1_school_seg_1", "w1_school_seg_6", "w1_school_seg_9",
                    "w1_school_seg_12", "w1_school_reg_1", "w1_school_reg_6",
                    "w1_school_reg_9", "w1_school_reg_12", "w2_school_cared_1",
                    "w2_school_cared_6", "w2_school_cared_9",
                    "w2_school_cared_12", "w2_students_clean_1", 
                    "w2_students_clean_6", "w2_students_clean_9", 
                    "w2_students_clean_12")

# check missing 
# Assess missingness in vars we want to impute / use for analysis
missingsummary <- data.frame(varname = impute.var.list, pctmiss = NA)
row.names(missingsummary) <- impute.var.list
for (i in impute.var.list){
  missingsummary[i, "pctmiss"] <- 100*sum(is.na(dat_imput[, i]))/nrow(dat_imput)
  
  print(i)
  print(table(dat_imput[, i], exclude = NULL))
}

missingordered <- missingsummary[order(missingsummary$pctmiss), ]
missingordered # up to 29.8% missingness in first grade "cared" variable
# all student composition vars are missing around 18%-20%
# TMM noting over 50% missing w2 deprsx

# save
write.xlsx(missingordered,
           paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/",
                  "results/missingordered_tab_v2.xlsx"))

ordered.var.list <- c(paste(missingordered$varname))

# data prep ----------------------------------------------------------
# Run single imputation
# prep data by dropping vars we don't need, ordering by missingness
impute.data <- dat_imput[, ordered.var.list]

# Set variable classes by type
str(impute.data)

# continuous vars
class(impute.data$w1_age_s)
class(impute.data$maternal_edu)
class(impute.data$paternal_edu)
class(impute.data$w1_edu_yrs)
class(impute.data$w1_tscore_s) # TMM adding first obs outcome
class(impute.data$w2_tscore_s) # TMM adding second obs outcome

# binary vars
impute.data$female <- as.factor(impute.data$female)
impute.data$usborn <- as.factor(impute.data$usborn)
impute.data$sbirth <- as.factor(impute.data$sbirth)
impute.data$concerned_thinking <- as.factor(impute.data$concerned_thinking)
impute.data$confidante <- as.factor(impute.data$confidante)
impute.data$pain <- as.factor(impute.data$pain)

impute.data$retire_fulltime <- as.factor(impute.data$retire_fulltime)
impute.data$retire_parttime<-as.factor(impute.data$retire_parttime)
impute.data$retire_notworking<- as.factor(impute.data$retire_notworking)
impute.data$school_att_clean_1 <- factor(impute.data$school_att_clean_1)
impute.data$school_att_clean_6 <- factor(impute.data$school_att_clean_6)
impute.data$school_att_clean_9 <- factor(impute.data$school_att_clean_9)
impute.data$school_att_clean_12 <- factor(impute.data$school_att_clean_12)

# categorical vars
impute.data$raceth <- factor(impute.data$raceth, ordered = F)
impute.data$maritalstatus <- factor(impute.data$maritalstatus, ordered = F)
impute.data$smoke_status<- factor(impute.data$smoke_status, ordered = F)
impute.data$maritalstatus <- factor(impute.data$maritalstatus, ordered = F)

# ordinal vars
impute.data$income <- factor(impute.data$income, ordered = T)
impute.data$vision <- factor(impute.data$vision, ordered = T)
impute.data$hearing <- factor(impute.data$hearing, ordered = T)
impute.data$sr_health <- factor(impute.data$sr_health, ordered = T) # TMM fixing

# recheck classes
str(impute.data)

# imputations ----------------------------------------------
ini <- mice(impute.data, maxit = 0,
            defaultMethod = c("pmm", "pmm", "pmm", "pmm"), seed = 12345)

ini$method
meth <- ini$method
meth

ini$predictorMatrix
pred <- ini$predictorMatrix

# run imputations
imp_pmm <- mice(impute.data, m = 30, maxit = 20, pred = pred, meth = meth,
                defaultMethod = c("pmm", "pmm", "pmm", "pmm"),
                seed = 12345)

# Save imputation object
saveRDS(imp_pmm,
        file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Data/",
                      "imp_pmm_v2.RDS"))

# Read in imputation object 
# imp_pmm <- readRDS(paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Data/",
                          # "imp_pmm_v2.RDS"))

# examine diagnostics 
imp_pmm$loggedEvents
p<- plot(imp_pmm)
print(p)
densityplot(imp_pmm, ~income)
densityplot(imp_pmm, ~w2_school_cared_1)
densityplot(imp_pmm, ~w2_school_cared_6)
densityplot(imp_pmm, ~w2_school_cared_9)
densityplot(imp_pmm, ~w2_school_cared_12)


# create stacked imputed dataset ---------------------------------------

imp.temp <- list()
for (i in 1:30){
  imp.temp[[i]] <- complete(imp_pmm, action = i)
  
  imp.temp[[i]] <- cbind(dat_imput$studyid, imp.temp[[i]][, c(impute.var.list)])
  imp.temp[[i]][, "imp"] <- i
}

dat_pmm_stacked <- do.call(rbind, imp.temp)
colnames(dat_pmm_stacked)
dat_pmm_stacked <- rename(dat_pmm_stacked, studyid=`dat_imput$studyid`)

# TMM adding line where we remove depr tscores (then add non-imputed ones later)
dat_pmm_stacked <- dat_pmm_stacked %>% 
  dplyr::select(-c(w1_tscore_s, w2_tscore_s))
# check
# colnames(dat_pmm_stacked)

# add outcome vars -- TMM changing to just tscores and age
health_outcomes <- dat_school_depr %>% 
  dplyr::select(c(studyid, w2_age_s, w1_tscore_s, w2_tscore_s))

dat_pmm_stacked <- dat_pmm_stacked[order(dat_pmm_stacked$studyid, 
                                         dat_pmm_stacked$imp),]
health_outcomes <- health_outcomes[order(health_outcomes$studyid),]

dat_pmm_stacked <- merge(x=dat_pmm_stacked, y=health_outcomes, by="studyid",
                         all.x=TRUE)

# Save stacked imputed data set
saveRDS(dat_pmm_stacked, 
     file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Data/",
                   "dat_pmm_stacked_v2.RDS"))
