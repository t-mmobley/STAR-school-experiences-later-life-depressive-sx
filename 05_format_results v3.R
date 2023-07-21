# Loading packages -------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "dplyr", 
       "survey", "tidyr", "jtools", "GGally", "VGAM", "huxtable", "mclust",
       "summarytools", "ggstance", "multcomp", "cowplot", "epiDisplay",
       "openxlsx", "ggpubr", "TraMineR", "cluster", "readxl","gridGraphics",
       "TraMineRextras", "mice", "mitools")

# Read -------------------------------------------------------------
# R1: TMM updating to output version 3
all_models <- 
  readRDS(file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/",
                      "results/all_models_gee_v3.rds"))

intx_results <- 
  readRDS(file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/",
                        "results/intx_results_gee_v3.rds"))

sa_models <- 
  readRDS(file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/",
                        "results/sa_models_gee_v3.rds"))

# format data --------------------------------------------------------

# change model names for sa results
# R1: TMM updating to 2 models instead of 3
sa_models$model <- 
  ifelse(sa_models$model=="m1", "sm1", 
         ifelse(sa_models$model=="m2", "sm2", NA))

# match colnames
all_models <- all_models %>% 
  rbind(sa_models) %>%
  dplyr::select(-c("se")) %>%
  rename(est = results,
         LL = `(lower`,
         UL = `upper)`,
         term = names)

rownames(all_models)<-c(1:132) # R1: TMM updating to new row number

intx_results$term <- "intx_term"
intx_results$missInfo <- NA
intx_results$n <- NA

# combine results 
res_all <- rbind(all_models, intx_results)

# filter results to school vars only 
spec_res <- res_all %>% filter(str_detect(term, "cared") | 
                                       str_detect(term, "students") |
                                       str_detect(term, "intx")) %>%
  mutate(grade = as.numeric(grade))

# order by grade and model
spec_res <- spec_res[
  with(spec_res, order(model, grade)),
  ]

# variables for figure ------------------------------------------------
# R1: TMM updating to models 2 and 3 
fig_res <- spec_res %>% filter(!str_detect(term, "1:w") & 
                                !str_detect(term, "cared") & 
                                model %in% c("m2", "m3"))

names(fig_res)[names(fig_res) == 'est'] <- "estimate"
names(fig_res)[names(fig_res) == 'LL'] <- "conf.low"
names(fig_res)[names(fig_res) == 'UL'] <- "conf.high"

fig_res$grade <- 
  ifelse(fig_res$grade==1,"Grade 1",
         ifelse(fig_res$grade==6, "Grade 6",
                ifelse(fig_res$grade==9, "Grade 9",
                       ifelse(fig_res$grade==12, "Grade 12", NA))))
# save
# R1: TMM updating output to version 3
write.xlsx(spec_res, 
           paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/results/",
                  "spec_res_gee_v3.xlsx"))

saveRDS(res_all, 
        file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/",
                      "results/res_all_gee_v3.rds"))

saveRDS(spec_res, 
        file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/",
                      "results/spec_res_gee_v3.rds"))

saveRDS(fig_res, 
        file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/Output/",
                      "results/fig_res_gee_v3.rds"))
