# Load ---------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "dplyr", 
       "survey", "tidyr", "jtools", "GGally", "VGAM", "huxtable", "mclust",
       "summarytools", "ggstance", "multcomp", "cowplot", "epiDisplay",
       "openxlsx", "ggpubr", "TraMineR", "cluster", "readxl","gridGraphics",
       "TraMineRextras", "mice", "mitools", "wesanderson")

# Read ---------------------------------------------------------------
# R1: TMM updating to output version 3
fig_res <- readRDS(file = paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/",
                              "Output/results/fig_res_gee_v3.rds"))

# Figure 1: EMM model results ----------------------------------------

# grade labels 
grade.labs <- c("Grade 1", "Grade 6", "Grade 9",
                "Grade 12")
names(grade.labs) <- c("Grade 1", "Grade 6", "Grade 9",
                       "Grade 12")

fig_res$label <- 
  ifelse(grepl("black",fig_res$term) & fig_res$model=="m2",
         "Mostly Black students\n (yes vs no)",
         ifelse(grepl("black",fig_res$term) & fig_res$model=="m3",
                "No caring teacher/staff:\n Mostly Black students\n (yes vs no)",
                "Caring teacher/staff:\n Mostly Black students\n (yes vs no)"))

fig_res$grade <- factor(fig_res$grade,      # Reordering group factor levels
                        levels = c("Grade 1", "Grade 6", "Grade 9",
                                   "Grade 12"))

fig_res$label <- 
  factor(fig_res$label,      # Reordering group factor levels
         levels = c("Caring teacher/staff:\n Mostly Black students\n (yes vs no)",
                    "No caring teacher/staff:\n Mostly Black students\n (yes vs no)",
                    "Mostly Black students\n (yes vs no)"))

dev.new(width = 6, height = 8, unit = "in")
fig1 <- ggplot(
  data=fig_res,
  aes(x = label, y = estimate,
      ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(data = fig_res, aes(shape = model, color = model), 
                  fatten = 6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = model),
                width = 0.2, cex = 1) +
  facet_grid(grade ~ ., scales = "free_y", labeller =
               labeller(grade = grade.labs)) +
  scale_color_manual(values = wes_palette("Moonrise3")) + 
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  ylab("Depressive symptoms") +
  scale_y_continuous(breaks = seq(from = -0.5, to = 0.5, by = 0.1)) +
  xlab('') +
  theme(legend.title = element_blank(), plot.title = element_text(size = 9),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text.y = element_text(hjust=0)) +
  coord_flip()
fig1

# R1: TMM updating output to version 3
ggsave(paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/",
       "Output/results/fig1_v3.tiff"), width = 7, height = 8, unit = c("in"),
       dpi = 300)
ggsave(paste0("C:/Users/tmobley/Box/STAR_School_seg_cared/",
              "Output/results/fig1_v3.jpg"), width = 7, height = 8, unit = c("in"),
       dpi = 300)
