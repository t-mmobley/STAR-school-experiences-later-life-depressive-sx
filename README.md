# STAR-school-experiences-later-life-depressive-sx
R code for "School racial composition, effect modification by having a caring adult at school , and later-life depressive symptoms: findings from the Study of Healthy Aging among African Americans" project

01_data_cleaning v2.R cleans the raw STAR data and prepares the data set for multiple imputation.

02_multiple_imputations v2.R implements multiple imputations with chained equations and saves a stacked data set with 30 imputations. 

03_MI_data_cleaning v3.R cleans the stacked imputed data set for regression and descriptive analyses.

04_school_comp_cared_deprsx_gee v3.R runs main nested GEE models stratified by grade and saves the results as a RDS file.

04b_school_comp_cared_deprsx_gee_sa v3 runs sensitivty analyses regression models stratified by grade and saves the results as a RDS file.

05_format_results v3.R formats result output for manuscript figures.

06_descr_analysis v2.R generates Table 1 in manuscript.

06_descr_analysis_grade_stratified v4.R generates grade-stratified descriptive Table 2 results.

07_results v3.R generates Figure 1 in the manuscript.
