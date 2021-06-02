
### model_select() ###

# The function that returns the table with the results of the tests that are necessary
# in the model choice. 
# It takes 4 arguments: fixed effects model object, random effects model object,
# POLS model object, and significance level ('sig.level') with default value set to 5%.


model_select <- function(fixed, random, pols, sig.level = 0.05) {
  
  library(dplyr)
  library(plm)
  
  
  # Breusch-Pagan Lagrange Multiplier test for random effects
  random_effects <- plmtest(pols, type=c("bp"))
  
  random_effects_con <- ifelse(random_effects$p.value < sig.level,
                               "significant random effects",
                               "insignificant random effects")
  
  # F test for individual fixed effects
  fixed_effects <- pFtest(fixed, pols)
  
  fixed_effects_con <- ifelse(fixed_effects$p.value < sig.level,
                              "significant fixed effects",
                              "insignificant fixed effects")
  
  # Breusch-Pagan Lagrange Multiplier test for time-fixed effects
  time_fixed_effects <- plmtest(fixed, c("time"), type=("bp"))
  
  time_fixed_effects_con <- ifelse(time_fixed_effects$p.value < sig.level,
                                   "time-fixed effects needed",
                                   "no time-fixed effects needed")
  
  # Hausmann test
  hausmann <- phtest(fixed, random)
  
  hausmann_con <- ifelse(hausmann$p.value < sig.level,
                         "choose fixed effects model",
                         "choose random effects model")
  
  
  # Data frame result
  result <- data.frame(
    test = c("Breusch-Pagan LM test for random effects",
             "F test for individual fixed effects",
             "Breusch-Pagan LM test for time-fixed effects",
             "Hausmann test"
             ),
    p.value = c(random_effects$p.value, fixed_effects$p.value, 
              time_fixed_effects$p.value, hausmann$p.value) %>% 
      round(4),
    conclusion = c(random_effects_con, fixed_effects_con,
                 time_fixed_effects_con, hausmann_con) ,
    row.names = NULL
    )
  
  result$p.value <- ifelse(result$p.value == 0, "< 0.0001" , result$p.value)
  
  
  return(result)

}