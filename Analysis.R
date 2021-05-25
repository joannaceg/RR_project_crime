
# Setting working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading necessary libraries
# library(MASS)
# library(sandwich)
# library(lmtest)
# library(Formula)
# library(stargazer)

library(bestNormalize)
library(dplyr)
library(GGally)
library(ggplot2)
library(plm)

data <- read.csv("data_no_missings.csv", header = T, sep = ";")

# needed ?
data$X <- NULL

# shortening names of the columns
colnames(data) <- c("Country", "Year", "Homicide", "Inequality", "GDP_per_capita",
                    "Lower_secondary_completion_rate", "RnD_expenditure", "School_enrollment", 
                    "Unemployment", "Urbanization_rate","Unsentenced", "Police")

summary(data)

# Huge plot, but may be interesting
ggpairs(data[,3:12], lower = list(combo = "box"), upper = list(combo = "blank"))


# Homicide transformation ----------------------------------------------------

ggplot(data) +
  geom_histogram(aes(Homicide), fill = "Steelblue", color = "black")

ggplot(data) +
  geom_histogram(aes(log(Homicide)), fill = "Steelblue", color = "black")

# We have to add sth to Homicide, not to lose 6 observations
# Adding 1 is too much in the case of this variable

quantile(data$Homicide, probs = seq(0,1,0.01))

data$Homicide %>% sort() %>% head(30)

# I decided arbitrarily to add 0.1 in log() function
# With this suggestion Box-Cox rather indicates log-transformation

bestNormalize::boxcox(data$Homicide + 0.1)$lambda

ggplot(data) +
  geom_histogram(aes(log(Homicide + 0.1)), fill = "Steelblue", color = "black")

# Therefore, we create the logarithm of the dependent variable
data$ln_Homicide <- log(data$Homicide + 0.1)




# We can apply Box-Cox transformation for some other variables and check the value of the optimal lambda.

### GDP per capita
bestNormalize::boxcox(data$GDP_per_capita)$lambda

ggplot(data) +
  geom_histogram(aes(GDP_per_capita), fill = "Steelblue", color = "black")

ggplot(data) +
  geom_histogram(aes(log(GDP_per_capita)), fill = "Steelblue", color = "black")

# We log-transform it
data$ln_GDP_per_capita <- log(data$GDP_per_capita)


### R & D expenditure
bestNormalize::boxcox(data$RnD_expenditure)$lambda

ggplot(data) +
  geom_histogram(aes(RnD_expenditure), fill = "Steelblue", color = "black")

ggplot(data) +
  geom_histogram(aes(log(RnD_expenditure)), fill = "Steelblue", color = "black")

# we can consider log-transformation


### Unemployment
bestNormalize::boxcox(data$Unemployment)$lambda

ggplot(data) +
  geom_histogram(aes(Unemployment), fill = "Steelblue", color = "black")

ggplot(data) +
  geom_histogram(aes(log(Unemployment)), fill = "Steelblue", color = "black")

# It is suggested to take it into log!
# However, we thought about putting it into interval as in your work



# Repetition -----------------------------------------------

# Choose vars, without RnD_expenditure and Urbanization_rate
df <- data %>%
  select(Country, Year, Homicide, Inequality, GDP_per_capita,
         Lower_secondary_completion_rate, School_enrollment, 
         Unemployment, Unsentenced, Police) %>%
  filter(Year %in% 2003:2015)


## Data transformation --------------------------------------

# Note: 0.1 is not truly repetition (probably)

# Homicide
bestNormalize::boxcox(df$Homicide + 0.1)$lambda

df$ln_Homicide <- log(df$Homicide + 0.1)

# GDP per capita
bestNormalize::boxcox(df$GDP_per_capita)$lambda

df$ln_GDP_per_capita <- log(df$GDP_per_capita)

# Unemployment
df$Unemployment_int <- cut(df$Unemployment,
                           breaks = c(0,5.5,8.5,Inf),
                           labels = c("low", "medium","high"))

table(df$Unemployment_int)
# Different frequency! :O 


library(corrplot)

correlations <- df %>%
  select(ln_Homicide, Inequality, ln_GDP_per_capita,
         Lower_secondary_completion_rate, School_enrollment, 
         Unemployment, Unsentenced, Police) %>%
  cor(use = "pairwise.complete.obs", method = c("pearson"))

corrplot.mixed(correlations,
               upper = "number",
               lower = "circle",
               tl.col= "black",
               tl.pos = "lt")

# quite different, but more or less the same coefficients


### Model --------------------------------------------

# fixed effects model
fixed <- plm(ln_Homicide ~ Inequality + ln_GDP_per_capita +
             Lower_secondary_completion_rate + School_enrollment + 
             Unemployment_int + Unsentenced + Police,
             data = df, 
             index=c("Country", "Year"),
             model="within")

summary(fixed)

# fixed, individual effects
fixef(fixed)

# random effects model
random <- plm(ln_Homicide ~ Inequality + ln_GDP_per_capita +
               Lower_secondary_completion_rate + School_enrollment + 
               Unemployment_int + Unsentenced + Police,
             data = df, 
             index=c("Country", "Year"),
             model="random")

summary(random)

# POLS
pols <- plm(ln_Homicide ~ Inequality + ln_GDP_per_capita +
              Lower_secondary_completion_rate + School_enrollment + 
              Unemployment_int + Unsentenced + Police,
            data = df,
            index=c("Country", "Year"),
            model="pooling")

# F test for individual effects for fixed model
pFtest(fixed, pols) 
# (checking whether fixed model is necessary at all;
# When we reject the null, it means that individual effects are significant)

# and individual effects for random effects
plmtest(pols, type=c("bp"))
# aslso effects are significant

# Testing for cross-sectional dependence/contemporaneous correlation:
# using Breusch-Pagan LM test of independence and Pasaran CD test
pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))
# we reject the null -> we have a cross sectional correlation

# Testing for serial correlation for fixed model
pbgtest(fixed)
# there is serial correlation
# we have to do some time series modeling ?

# Testing for serial correlation for random model
pbgtest(random)
# the same...

# Testing for heteroskedasticity
bptest(ln_Homicide ~ Inequality + ln_GDP_per_capita +
         Lower_secondary_completion_rate + School_enrollment + 
         Unemployment_int + Unsentenced + Police,
       data = df,
       studentize=F)


# hausmann test
phtest(fixed, random)
# reject the null -> we choose fixed effects model




