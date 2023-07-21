# Loading packages -------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "dplyr", 
       "survey", "tidyr", "jtools", "GGally", "VGAM", "huxtable", 
       "summarytools", "ggstance", "multcomp", "cowplot", "geepack",
       "openxlsx", "ggpubr","readxl","gridGraphics","mice", "mitools")

options(scipen = 100)

# Read -------------------------------------------------------------
analysis_dat <- readRDS(paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/",
            "Data/analysis_dat_v2.RDS"))

# source scripts 
source(paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Code/",
              "STAR_school_late_life_health/intx_function.R"))

# Nested gee models ---------------------------------------------------

# interaction estimate results
# R1: TMM updating to call model 3 (see changes below)
intx_results <- tibble("grade" = c(1, 6, 9, 12),
                      "model" = rep(c("m3"), 4),
                      "est" = 0, "LL" = 0, "UL" = 0)

# Nested linear model matrix 
# R1: TMM adding maternal edu and paternal edu to fully-adjusted models
# R1: TMM removing model that adjusted for "cared" variable but no intx (prev m3)
for(i in c(1,6,9,12)){
if(!exists("model_matrix")){
  model_matrix <-
    tibble("grade" = rep(c(paste0(i)),3),
         "model_name" = c("m1", "m2", "m3"), 
         "model" = 
           c(paste0("deprsx_tscore1sd_s ~ agewav65_s + female + ",
                    "w2_students_black_",i),
             paste0("deprsx_tscore1sd_s ~ agewav65_s + female + sbirth + ",
                    "maternal_edu8 + paternal_edu8 + w2_students_black_",i),
             paste0("deprsx_tscore1sd_s ~ agewav65_s + female +  sbirth +",
                    "maternal_edu8 + paternal_edu8 + w2_students_black_",i,
                    " + w2_school_cared_",i,"*w2_students_black_",i)))
  } else{
  temp <- 
    tibble("grade" = rep(c(paste0(i)), each=3),
           "model_name" = rep(c("m1", "m2", "m3")), 
           "model" = 
             c(paste0("deprsx_tscore1sd_s ~ agewav65_s + female + ",
                      "w2_students_black_",i),
               paste0("deprsx_tscore1sd_s ~ agewav65_s + female + sbirth + ",
                      "maternal_edu8 + paternal_edu8 + w2_students_black_",i),
               paste0("deprsx_tscore1sd_s ~ agewav65_s + female +  sbirth + ",
                      "maternal_edu8 + paternal_edu8 + w2_students_black_",i,
                      " + w2_school_cared_",i,"*w2_students_black_",i)))
  model_matrix <- rbind(model_matrix, temp)
  }
}

for(model in unique(model_matrix$model)){
  formula <- unlist(model_matrix[which(model_matrix$model == model), 
                                 "model"])
  
  grade <- unlist(model_matrix[which(model_matrix$model == model), 
                                 "grade"])
  
  model_name <- unlist(model_matrix[which(model_matrix$model == model), 
                               "model_name"])
  
  model_list <- list()
  for(i in 1:max(analysis_dat$imp)){
      subset <- analysis_dat %>% filter(imp == i)
      
      model_list[[i]] <- 
        geeglm(formula(paste(formula), collapse=" "), 
               id = studyid, 
               data = subset,
               family = gaussian(link="identity"),
               corstr = "exchangeable",
               std.err = 'san.se') # robust ses
    }
    
    coef.names <- names(model_list[[i]]$coefficients)
    stuff <- summary(MIcombine(model_list))
    
    #---- intx est ----
    # R1: TMM updating to new model 3
    if(model_name=="m3"){
    MI_model <- MIcombine(model_list)
  
    intx_results[which(intx_results$grade == grade &
                        intx_results$model == "m3"),
                c("est", "LL", "UL")] <- 
      t(custom_est_calc(1, grade, MI_model))
    }
    
    # R1: TMM adding code snippet to print intx p-value for supp table
    if(model_name=="m3"){
      pval <- summary(pool(model_list))
      print(pval[9,])
    }
    
  if(!exists("all_models")){
    assign("all_models", stuff %>%
             mutate("grade" = grade, "model" = model_name, 
                    "names" = coef.names, 
                    "n" = length(unique(analysis_dat$studyid))))
    } else{
      all_models <- 
        rbind(all_models, stuff %>% 
              mutate("grade" = grade, "model" = model_name, 
                     "names" = coef.names, 
                     "n" = length(unique(analysis_dat$studyid))))
    }
}

# save results -------------------------------------------------------
# R1: TMM updating output fil names to version 3 (script version)
saveRDS(all_models, file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/",
        "Output/results/all_models_gee_v3.rds"))

saveRDS(intx_results, 
        file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/",
                                  "results/intx_results_gee_v3.rds"))
