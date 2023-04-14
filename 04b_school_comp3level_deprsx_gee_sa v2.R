# Loading packages -------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "dplyr", 
       "survey", "tidyr", "jtools", "GGally", "VGAM", "huxtable", 
       "summarytools", "ggstance", "multcomp", "cowplot", "geepack",
       "openxlsx", "ggpubr","readxl","gridGraphics","mice", "mitools", "rlang")

options(scipen = 100)

# Read -------------------------------------------------------------
analysis_dat <- readRDS(paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/",
                               "Data/analysis_dat_v2.RDS"))

# source scripts -- not using TMM 3/27 [TMM moved script to old folder]
# source(paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Code/",
#               "STAR_school_late_life_health/intx_function v2.R"))

# set reference levels
analysis_dat$w2_students_3cat_1 <- 
  relevel(analysis_dat$w2_students_3cat_1, ref = 2)
analysis_dat$w2_students_3cat_6 <- 
  relevel(analysis_dat$w2_students_3cat_6, ref = 2)
analysis_dat$w2_students_3cat_9 <- 
  relevel(analysis_dat$w2_students_3cat_9, ref = 2)
analysis_dat$w2_students_3cat_12 <- 
  relevel(analysis_dat$w2_students_3cat_12, ref = 2)


# Nested gee models ---------------------------------------------------

# interaction estimate results -- not including for now
# intx_results <- tibble("grade" = c(1, 6, 9, 12),
#                        "model" = rep(c("m4"), 4),
#                        "est" = 0, "LL" = 0, "UL" = 0)

# Nested linear model matrix 
for(i in c(1,6,9,12)){
  if(!exists("model_matrix")){
    model_matrix <-
      tibble("grade" = rep(c(paste0(i)), 3),
             "model_name" = c("m1", "m2", "m4"), 
             "model" = 
               c(paste0("deprsx_tscore1sd_s ~ agewav65_s + female + ",
                        "w2_students_3cat_",i),
                 paste0("deprsx_tscore1sd_s ~ agewav65_s + female + sbirth + ",
                        "w2_students_3cat_",i),
                 paste0("deprsx_tscore1sd_s ~ agewav65_s + female +  sbirth +",
                        " w2_students_3cat_",i,
                        " + w2_school_cared_",i,"*w2_students_3cat_",i)))
  } else{
    temp <- 
      tibble("grade" = rep(c(paste0(i)), each = 3),
             "model_name" = rep(c("m1", "m2", "m4")), 
             "model" = 
               c(paste0("deprsx_tscore1sd_s ~ agewav65_s + female + ",
                        "w2_students_3cat_",i),
                 paste0("deprsx_tscore1sd_s ~ agewav65_s + female + sbirth + ",
                        "w2_students_3cat_",i),
                 paste0("deprsx_tscore1sd_s ~ agewav65_s + female +  sbirth +",
                        " w2_students_3cat_",i,
                        " + w2_school_cared_",i,"*w2_students_3cat_",i)))
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
  
  dat <- analysis_dat %>% 
    filter(!is.na(eval(parse_expr(paste0("w2_students_3cat_", grade)))))
  
  model_list <- list()
  for(i in 1:max(analysis_dat$imp)){
    subset <- dat %>% filter(imp == i)
    
    model_list[[i]] <- 
      geeglm(formula(paste(formula), collapse=" "), 
             id = studyid, 
             data = subset,
             family = gaussian(link="identity"),
             corstr = "exchangeable",
             std.err = 'san.se',
             na.action = na.pass) # robust ses
  }
  
  coef.names <- names(model_list[[i]]$coefficients)
  stuff <- summary(MIcombine(model_list))
  
  # ---- intx est ----
  # if(model_name=="m4"){
  #   MI_model <- MIcombine(model_list)
  # 
  #   intx_results[which(intx_results$grade == grade &
  #                        intx_results$model == "m4"),
  #                c("est", "LL", "UL")] <-
  #     t(custom_est_calc(1, '3cat', grade, MI_model))
  # }
  
  if(!exists("all_models")){
    assign("all_models", stuff %>%
             mutate("grade" = grade, "model" = model_name, 
                    "names" = coef.names, 
                    "n" = length(unique(dat$studyid))))
  } else{
    all_models <- 
      rbind(all_models, stuff %>% 
              mutate("grade" = grade, "model" = model_name, 
                     "names" = coef.names,
                     "n" = length(unique(dat$studyid))))
  }
}

# save results -------------------------------------------------------
saveRDS(all_models, file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/",
                                  "Output/results/sa_models_gee_v2.rds"))
