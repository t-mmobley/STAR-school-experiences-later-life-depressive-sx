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
intx_results <- tibble("grade" = c(1, 6, 9, 12),
                      "model" = rep(c("m4"), 4),
                      "est" = 0, "LL" = 0, "UL" = 0)

# Nested linear model matrix 
for(i in c(1,6,9,12)){
if(!exists("model_matrix")){
  model_matrix <-
    tibble("grade" = rep(c(paste0(i)),4),
         "model_name" = c("m1", "m2", "m3", "m4"), 
         "model" = 
           c(paste0("deprsx_tscore1sd_s ~ agewav65_s + female + ",
                    "w2_students_black_",i),
             paste0("deprsx_tscore1sd_s ~ agewav65_s + female + sbirth + ",
                    "w2_students_black_",i),
             paste0("deprsx_tscore1sd_s ~ agewav65_s + female +  sbirth + ",
                    "w2_school_cared_",i," + w2_students_black_",i),
             paste0("deprsx_tscore1sd_s ~ agewav65_s + female +  sbirth +",
                    " w2_students_black_",i,
                    " + w2_school_cared_",i,"*w2_students_black_",i)))
  } else{
  temp <- 
    tibble("grade" = rep(c(paste0(i)), each=4),
           "model_name" = rep(c("m1", "m2", "m3", "m4")), 
           "model" = 
             c(paste0("deprsx_tscore1sd_s ~ agewav65_s + female + ",
                      "w2_students_black_",i),
               paste0("deprsx_tscore1sd_s ~ agewav65_s + female + sbirth + ",
                      "w2_students_black_",i),
               paste0("deprsx_tscore1sd_s ~ agewav65_s + female +  sbirth + ",
                      "w2_school_cared_",i," + w2_students_black_",i),
               paste0("deprsx_tscore1sd_s ~ agewav65_s + female +  sbirth + ",
                      "w2_students_black_",i,
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
    if(model_name=="m4"){
    MI_model <- MIcombine(model_list)
  
    intx_results[which(intx_results$grade == grade &
                        intx_results$model == "m4"),
                c("est", "LL", "UL")] <- 
      t(custom_est_calc(1, grade, MI_model))
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
saveRDS(all_models, file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/",
        "Output/results/all_models_gee_v2.rds"))

saveRDS(intx_results, 
        file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/",
                                  "results/intx_results_gee_v2.rds"))
