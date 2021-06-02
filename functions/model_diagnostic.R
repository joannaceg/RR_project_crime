
### model_diagnostic() ###

# The function that returns the table with the results of the tests that are crucial
# in the model diagnostic, including tests for: normality of residuals, cross-sectional dependence,
# serial correlation and heteroskedasticity.

# It takes 2 arguments: model object and significance level ('sig.level') with default value set to 5%.


model_diagnostic <- function(model, sig.level = 0.05) {
  
  library(dplyr)
  library(lmtest)
  library(plm)
  library(tseries)
  
  
  # Jarque-Bera LM test for normality of residuals
  jarque_bera <- jarque.bera.test(model$residuals)
  
  jarque_bera_con <- ifelse(jarque_bera$p.value < sig.level,
                            "not normally distributed residuals", "normally distributed residuals")
  
  # Breusch-Pagan LM test for cross-sectional dependence
  cross_sectional_BP <- pcdtest(model, test = c("lm"))
  
  cross_sectional_BP_con <- ifelse(cross_sectional_BP$p.value < sig.level,
                                   "cross-sectional dependence", "no cross-sectional dependence")
  
  # Pesaran CD test for cross-sectional dependence
  cross_sectional_P <- pcdtest(model, test = c("cd"))
  
  cross_sectional_P_con <- ifelse(cross_sectional_P$p.value < sig.level,
                                  "cross-sectional dependence", "no cross-sectional dependence")
  
  # Breusch-Godfrey/Wooldridge test for serial correlation
  serial_correlation <- pbgtest(model)
  
  serial_correlation_con <- ifelse(serial_correlation$p.value < sig.level,
                                   "serial correlation", "no serial correlation")
  
  # Breusch-Pagan test for heteroskedasticity
  heteroskedasticity <- bptest(model$formula,
                               data = model$model,
                               studentize=F)
  
  heteroskedasticity_con <- ifelse(heteroskedasticity$p.value < sig.level,
                                   "heteroskedasticity", "homoskedasticity")
  
  
  # Data frame result
  result <- data.frame(
    test = c("Jarque-Bera LM test for normality of residuals",
             "Breusch-Pagan LM test for cross-sectional dependence",
             "Pesaran CD test for cross-sectional dependence",
             "Breusch-Godfrey/Wooldridge test for serial correlation",
             "Breusch-Pagan test for heteroskedasticity"
    ),
    p.value = c(jarque_bera$p.value, cross_sectional_BP$p.value, cross_sectional_P$p.value, 
                serial_correlation$p.value, heteroskedasticity$p.value) %>% 
      round(4),
    conclusion = c(jarque_bera_con, cross_sectional_BP_con, cross_sectional_P_con,
                   serial_correlation_con, heteroskedasticity_con),
    row.names = NULL
  )
  
  result$p.value <- ifelse(result$p.value == 0, "< 0.0001" , result$p.value)
  
  
  return(result)
  
}

