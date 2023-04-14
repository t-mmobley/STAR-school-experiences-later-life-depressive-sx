custom_est_calc <- function(cared, grade, model){
  pt_est <- coef(model)[paste0("w2_students_black_",grade,"1")] + 
    coef(model)[paste0("w2_students_black_",grade,
                       "1:w2_school_cared_",grade,"1")]*
    cared
  
  sd <- 
    sqrt(vcov(model)[paste0("w2_students_black_",grade,"1"),
                     paste0("w2_students_black_",grade,"1")] + 
           (cared)^2*
           vcov(model)[paste0("w2_students_black_",grade,
           "1:w2_school_cared_",grade,"1"), 
                       paste0("w2_students_black_",grade,
                              "1:w2_school_cared_",grade,"1")] + 
           2*cared*
           vcov(model)[paste0("w2_students_black_",grade,"1"), 
                       paste0("w2_students_black_",grade,
                              "1:w2_school_cared_",grade,"1")])
  
  ll <- pt_est - 1.96*sd
  ul <- pt_est + 1.96*sd
  
  return(c(pt_est, ll, ul))
}
