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
#Getting values for the imputed dataset; there should be no missingness!

for(j in c("w2_students_black_1", "w2_students_black_6","w2_students_black_9",
           "w2_students_black_12")){

T1results_cat_impute<-matrix(nrow=1, ncol=3) 
T1results_cat_impute[1,]<- c(j, table(eval(parse_expr(paste0("baseline_dat$",j))),
                                                       exclude=NULL)/30) # 739

# TMM moving catvars vector here so it's regenerated for each grade
catvars <- c("female", "sbirth", "usborn", "partnered", "retired", "confidante",
             "maternal_edu8", "paternal_edu8", "fairpoor_health", "ever_smoke",
             "pain","concerned_thinking", "fairpoor_hearing", "fairpoor_vision")

  # TMM adding code chunk to add school vars to table 4/10
  if(j=="w2_students_black_1"){
    catvars <- c(catvars,'w2_school_cared_1', 'w1_school_reg_1',
                    'w1_school_seg_1')
  } 
  if(j=="w2_students_black_6"){
    catvars <- c(catvars,'w2_school_cared_6', 'w1_school_reg_6',
                      'w1_school_seg_6')
  }
  if(j=="w2_students_black_9"){
    catvars <- c(catvars,'w2_school_cared_9', 'w1_school_reg_9',
                    'w1_school_seg_9')
  } 
  if(j=="w2_students_black_12"){
    catvars <- c(catvars,'w2_school_cared_12', 'w1_school_reg_12',
                      'w1_school_seg_12') 
  }


for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("baseline_dat$",catvars[i]))),
                    eval(parse_expr(paste0("baseline_dat$",j))),
                    exclude=NULL)/30
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat_impute<-rbind(T1results_cat_impute, c(paste(catvars[i]), 
                                                      rep(NA,2))) 
  T1results_cat_impute<-rbind(T1results_cat_impute,cbind(labs, tab.to.add))
}
#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error)

colnames(T1results_cat_impute)<-c("Variablename", "n_0", "n_1") 
rownames(T1results_cat_impute)<-NULL
T1results_cat_impute<-as.data.frame(T1results_cat_impute)

#Get %'s by race/ethnicity
T1results_prop_impute<-matrix(nrow=1, ncol=3)
T1results_prop_impute[1,]<- c(j, 
                              as.numeric(T1results_cat_impute[1,2])/739,
                              as.numeric(T1results_cat_impute[1,3])/739)

Margins<-as.numeric(table(eval(parse_expr(paste0("baseline_dat$",j))),
                          exclude=NULL))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("baseline_dat$",catvars[i]))),
                        eval(parse_expr(paste0("baseline_dat$",j))), 
                        exclude=NULL))/Margins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop_impute<-rbind(T1results_prop_impute, c(paste(catvars[i]),
                                                        rep(NA,2))) 
  T1results_prop_impute<-rbind(T1results_prop_impute,cbind(labs, tab.to.add))
}

colnames(T1results_prop_impute)<-c("Variablename", "prop 0", "prop 1") 
rownames(T1results_prop_impute)<-NULL
T1results_prop_impute<-as.data.frame(T1results_prop_impute)

#merge n and % results
T1results_cat_impute<-left_join(T1results_cat_impute, 
                                T1results_prop_impute, by="Variablename")

#numeric columns + format prop column
T1results_cat_format <- T1results_cat_impute %>%
  mutate(n_0 = as.numeric(n_0),
         n_1 = as.numeric(n_1),
         `prop 0` = as.numeric(`prop 0`),
         `prop 1` = as.numeric(`prop 1`)) %>%
  mutate(`prop 0` = round((`prop 0`*100),1),
         `prop 1` = round((`prop 1`*100),1),
          n_0 = round(n_0,0),
          n_1 = round(n_1, 0)) %>%
  mutate(`n_0 (%)` = paste0(n_0, " (",`prop 0`, ")"),
         `n_1 (%)` = paste0(n_1, " (",`prop 1`, ")")) %>%
  dplyr::select("Variablename", "n_0 (%)", "n_1 (%)") %>%
  mutate(`n_0 (%)` = if_else(grepl("(NA)",`n_0 (%)`), " ", `n_0 (%)`),
         `n_1 (%)` = if_else(grepl("(NA)",`n_1 (%)`), " ", `n_1 (%)`))

  if(!exists("all_catres")){
    assign("all_catres", T1results_cat_format %>%
           mutate("grade" = j))
  } else{
    all_catres <- 
      rbind(all_catres, T1results_cat_format %>% 
            mutate("grade" = j))
  }
}

# continuous vars ----------------------------------------------------
# TMM updating after YW did code review
cont_results <- tibble()
for(j in c("w2_students_black_1", "w2_students_black_6","w2_students_black_9",
           "w2_students_black_12")){
  # Since it's stratified by an imputed variable, stats from each imputation is needed
  temp_cont_tib <- data.frame()
  for (i in 1:30){
    temp_cont_tib <- temp_cont_tib %>%
      rbind(baseline_dat %>% filter(imp == i) %>%
              ungroup() %>%
              group_by(!!sym(j)) %>%
              summarise(
                mean_edu_imp = mean(w1_edu_yrs),
                mean_age_imp = mean(w1_age_s),
                var_edu_imp = var(w1_edu_yrs),
                var_age_imp = var(w1_age_s)) %>%
              mutate(imp = i))
  }
  
  temp_cont_imputed <- temp_cont_tib %>%
    group_by(!!sym(j)) %>%
    summarise(mean_edu = mean(mean_edu_imp),
              mean_age = mean(mean_age_imp),
              sd_age = sqrt(mean(var_age_imp)),
              sd_edu = sqrt(mean(var_edu_imp))) %>%
    pivot_longer(!all_of(j), names_to = c("stat", "var"),
                 values_to = "value",
                 names_pattern = "(.*)_(.*)") %>%
    mutate(group = paste0(j, !!sym(j))) %>%
    dplyr::select(group, var, stat, value)
  
  cont_results <- rbind(cont_results, temp_cont_imputed)
  
}

cont_results <- cont_results %>%
  mutate(group = factor(group, levels = c("w2_students_black_11",
                                          "w2_students_black_10",
                                          "w2_students_black_61",
                                          "w2_students_black_60",
                                          "w2_students_black_91",
                                          "w2_students_black_90",
                                          "w2_students_black_121",
                                          "w2_students_black_120"))) %>%
  arrange(group, var, stat)

T1_res <- list(cat_res = all_catres,
               cont_res = cont_results)

write.xlsx(T1_res, 
        paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/results/",
               "T1results_gradestrat_v2.xlsx"))
saveRDS(T1_res, paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/",
                       "results/T1results_gradestrat_v2.RDS"))
