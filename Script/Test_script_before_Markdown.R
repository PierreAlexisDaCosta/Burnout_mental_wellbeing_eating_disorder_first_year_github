

Reponse_EBE_P1_for_summary <-
  cbind(Reponse_EBE_P1_for_summary_withoutscore, All_scores)

Reponse_EBE_P1_for_summary 

# univariate_lm_analysis <-
#   function(data, var_to_explain, explanatory_vars){
#     explanatory_vars %>%       # begin with variables of interest
#       str_c(paste0(var_to_explain, " ~ "), .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
#       # iterate through each univariate formula
#       map(                               
#         .f = ~lm(                       # pass the formulas one-by-one to glm()
#           formula = as.formula(.x),      # within glm(), the string formula is .x
#           data = data)) %>%       # dataset
#       # tidy up each of the glm regression outputs from above
#       map(
#         .f = ~tbl_regression(
#           .x)) %>%
#       tbl_stack()
#   }
# univariate_lm_analysis <- function(data, var_to_explain, explanatory_vars) {
#   results <- explanatory_vars %>% 
#     map(function(x) {
#       formula <- as.formula(paste(var_to_explain, "~", x))
#       model <- lm(formula, data = data)
#       tidied <- tidy(model) %>% filter(term != "(Intercept)")
#       r2 <- summary(model)$r.squared
#       tidied %>%
#         mutate(
#           response = var_to_explain,
#           explanatory = x,
#           r_squared = r2
#         )
#     })
#   
#   bind_rows(results)
# }
# 
# univariate_lm_analysis_m1 <-
#   function(data, var_to_explain, explanatory_vars){
#     explanatory_vars %>%       # begin with variables of interest
#       str_c(paste0(var_to_explain, " ~ Gender + Age + Current_track + University"), .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
#       # iterate through each univariate formula
#       map(                               
#         .f = ~lm(                       # pass the formulas one-by-one to glm()
#           formula = as.formula(.x),      # within glm(), the string formula is .x
#           data = data)) %>%       # dataset
#       # tidy up each of the glm regression outputs from above
#       map(
#         .f = ~tbl_regression(
#           .x)) %>%
#       tbl_stack()
#   } # Il faut rajouter l'Université pour ce modèle
# 
# univariate_lm_analysis_m2_SCOFF <-
#   function(data, var_to_explain, explanatory_vars){
#     explanatory_vars %>%       # begin with variables of interest
#       str_c(paste0(var_to_explain, " ~ Academic_Efficacy + Cynicism + Emotional_Exhaustion + Warwick_Edinburg_total"), .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
#       # iterate through each univariate formula
#       map(                               
#         .f = ~lm(                       # pass the formulas one-by-one to glm()
#           formula = as.formula(.x),      # within glm(), the string formula is .x
#           data = data)) %>%       # dataset
#       # tidy up each of the glm regression outputs from above
#       map(
#         .f = ~tbl_regression(
#           .x)) %>%
#       tbl_stack()
#   }

library(broom)


clm_SCOFF_m1 <-
  multivariate_clm_analysis_m1AgeGender(data = Reponse_EBE_P1_for_regression_recoded, 
                                        var_to_explain = "SCOFF_total", 
                                        explanatory_vars =
                                          colnames(
                                            Reponse_EBE_P1_for_regression_recoded %>%
                                              dplyr::select(-c("SCOFF_total", "Age", "Gender", "Current_track"))))

clm_SCOFF_m2 <-
  multivariate_clm_analysis_m2Burnout_wellbeing(data = Reponse_EBE_P1_for_regression_recoded, 
                                                var_to_explain = "SCOFF_total", 
                                                explanatory_vars =
                                                  colnames(
                                                    Reponse_EBE_P1_for_regression_recoded %>%
                                                      dplyr::select(-c("SCOFF_total", "Academic_Efficacy", 
                                                                       "Cynicism",
                                                                       "Emotional_Exhaustion", "Warwick_Edinburg_total"))))


Reponse_EBE_P1_for_regression_recoded$Tutoring_participation_influence

univariate_lm_analysis(data = Reponse_EBE_P1_for_regression_recoded, 
                       var_to_explain = "Warwick_Edinburg_total", 
                       explanatory_vars =
                         colnames(
                           Reponse_EBE_P1_for_regression_recoded %>%
                             dplyr::select(-c("Warwick_Edinburg_total"))))
lm_multivariateWE_m1 <-
  multivariate_lm_analysis_m1AgeGender(data = Reponse_EBE_P1_for_regression_recoded, 
                                       var_to_explain = "Warwick_Edinburg_total", 
                                       explanatory_vars =
                                         colnames(
                                           Reponse_EBE_P1_for_regression_recoded %>%
                                             dplyr::select(-c("Warwick_Edinburg_total", "Age", "Gender", "Current_track"))))



Reponse_EBE_P1_summary








Reponse_EBE_P1_summary_gender




Reponse_EBE_P1_for_regression_recoded$Academic_Efficacy
Reponse_EBE_P1_for_regression_recoded$Emotional_Exhaustion
Reponse_EBE_P1_for_regression_recoded$Cynicism


Reponse_EBE_P1_for_regression_recoded$Warwick_Edinburg_total <-
  as.numeric(Reponse_EBE_P1_for_regression_recoded$Warwick_Edinburg_total )

lm_univariateWE <-
  univariate_lm_analysis(data = Reponse_EBE_P1_for_regression_recoded, 
                         var_to_explain = "Warwick_Edinburg_total", 
                         explanatory_vars =
                           colnames(
                             Reponse_EBE_P1_for_regression_recoded %>%
                               dplyr::select(-c("Warwick_Edinburg_total"))))


lm_multivariateWE_m1 <-
  multivariate_lm_analysis_m1AgeGender(data = Reponse_EBE_P1_for_regression_recoded, 
                                       var_to_explain = "Warwick_Edinburg_total", 
                                       explanatory_vars =
                                         colnames(
                                           Reponse_EBE_P1_for_regression_recoded %>%
                                             dplyr::select(-c("Warwick_Edinburg_total", "Age", "Gender", "Current_track"))))


lm_multivariateWE_m2 <-
  multivariate_lm_analysis_m2_forWarwick_Edinburg_total(data = Reponse_EBE_P1_for_regression_recoded, 
                                                        var_to_explain = "Warwick_Edinburg_total", 
                                                        explanatory_vars =
                                                          colnames(
                                                            Reponse_EBE_P1_for_regression_recoded %>%
                                                              dplyr::select(-c("SCOFF_total", "Academic_Efficacy", 
                                                                               "Cynicism",
                                                                               "Emotional_Exhaustion", "Warwick_Edinburg_total"))))

tbl_SCOFF_merged <-
  tbl_merge(
    tbls = list(
      lm_univariateWE,
      lm_multivariateWE_m1,
      lm_multivariateWE_m2
    ),
    tab_spanner = c("**Univariate lm (Warwick Edinburg)**",
                    "**Multivariate lm m1: + Age + Gender + Current_track**",
                    "**Multivariate lm m2: + Burnout + TCA**")
  )














