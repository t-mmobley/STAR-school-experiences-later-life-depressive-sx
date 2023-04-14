#Loading packages -----------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "dplyr", 
       "survey", "tidyr", "jtools", "GGally", "VGAM", "huxtable", "mclust",
       "summarytools", "ggstance", "multcomp", "cowplot", "epiDisplay",
       "openxlsx", "ggpubr", "TraMineR", "cluster", "readxl","gridGraphics",
       "TraMineRextras", "mice", "mitools", "rlang")

options(scipen = 100)

# Read ----------------------------------------------------------------
analysis_dat <- readRDS(paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/",
                               "Data/analysis_dat_v2.RDS"))

# check number of people with two depression measures
# check <- analysis_dat %>% filter(imp==1) # 1107 obs
# check %>% group_by(analysis_wave) %>% count() # 739 w1 and 368 w2 (in this analysis)
# summary(analysis_dat$w1_age_s) # range 53.08-90.00

# keep everyone's first observation 
baseline_dat <- analysis_dat %>%
  arrange(studyid, analysis_wave, imp) %>%
  distinct(studyid, imp, .keep_all = TRUE) 

# table(baseline_dat$analysis_wave, exclude = NULL) # all w1

# categorical vars -----------------------------------------------------
catvars <- c("female", "sbirth", "usborn", "partnered", "retired", "confidante",
             "maternal_edu8", "paternal_edu8", "fairpoor_health", "ever_smoke",
             "pain","concerned_thinking", "fairpoor_hearing", "fairpoor_vision",
             "school_att_clean_1", "school_att_clean_6", 
             "school_att_clean_9", "school_att_clean_12", 
             "w1_school_seg_1", "w1_school_seg_6", "w1_school_seg_9",
             "w1_school_seg_12","w1_school_reg_1","w1_school_reg_6",
             "w1_school_reg_9", "w1_school_reg_12", "w2_school_cared_1", 
             "w2_school_cared_6", "w2_school_cared_9", "w2_school_cared_12",
             "w2_students_clean_1","w2_students_clean_6", "w2_students_clean_9",
             "w2_students_clean_12", "w1_school_south_1", "w1_school_south_6",
             "w1_school_south_9", "w1_school_south_12", "w2_students_black_1", 
             "w2_students_black_6","w2_students_black_9","w2_students_black_12")

#Getting values for the imputed dataset; there should be no missingness!
T1results_cat_impute<-matrix(nrow=1, ncol=2) 
baseline_dat %>% filter(imp==1) %>% count() # 739
T1results_cat_impute[1,]<- c("Sample total (n)", 22170/30) # 739

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("baseline_dat$",catvars[i]))), 
                    exclude=NULL)/30
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat_impute<-rbind(T1results_cat_impute, c(paste(catvars[i]), 
                                                      rep(NA,1))) 
  T1results_cat_impute<-rbind(T1results_cat_impute,cbind(labs, tab.to.add))
}
#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error)

colnames(T1results_cat_impute)<-c("Variablename", "n") 
rownames(T1results_cat_impute)<-NULL
T1results_cat_impute<-as.data.frame(T1results_cat_impute)

#Get %'s by race/ethnicity
T1results_prop_impute<-matrix(nrow=1, ncol=2)
T1results_prop_impute[1,]<- c("Sample total (n)", 739/739)

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("baseline_dat$",catvars[i])))
                        , exclude=NULL))/22170)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop_impute<-rbind(T1results_prop_impute, c(paste(catvars[i]),
                                                        rep(NA,1))) 
  T1results_prop_impute<-rbind(T1results_prop_impute,cbind(labs, tab.to.add))
}

colnames(T1results_prop_impute)<-c("Variablename", "prop") 
rownames(T1results_prop_impute)<-NULL
T1results_prop_impute<-as.data.frame(T1results_prop_impute)

#merge n and % results
T1results_cat_impute<-left_join(T1results_cat_impute, 
                                T1results_prop_impute, by="Variablename")

#numeric columns + format prop column
T1results_cat_format <- T1results_cat_impute %>%
  mutate(n = as.numeric(n),
         prop = as.numeric(prop)) %>%
  mutate(prop = round((prop*100),1),
         n = round(n,0)) %>%
  mutate(`n (%)` = paste0(n, " (",prop, ")")) %>%
  dplyr::select("Variablename", "n (%)") %>%
  mutate(`n (%)` = if_else(grepl("(NA)",`n (%)`), " ", `n (%)`))

# continuous vars ----------------------------------------------------
cont_results <- 
  tibble("var" = 
           rep(c("w1_age_s", "w1_edu_yrs"), 
               each = 2), 
         "stat" = rep(c("mean", "SD"), 2), 
         "overall" = 0)

for(var in c("w1_age_s", "w1_edu_yrs")){
  subset <- baseline_dat %>% 
    dplyr::select("studyid", "imp", all_of(var)) %>%
    pivot_wider(id_cols = studyid, names_from = imp, values_from = var)
  
  subset <- as.matrix(subset[, 2:31])
  imp_mean = rowMeans(subset)
  cont_results[which(cont_results$var == var & cont_results$stat == "mean"), 
               "overall"] <- mean(imp_mean)
  cont_results[which(cont_results$var == var & cont_results$stat == "SD"), 
               "overall"] <- sd(imp_mean)
  }

T1_res <- list(cat_res = T1results_cat_format,
               cont_res = cont_results)

write.xlsx(T1_res, 
        paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/results/",
               "T1results_v2.xlsx"))
saveRDS(T1_res, paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/",
                       "results/T1results_v2.RDS"))
