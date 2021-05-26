
# Setting working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading necessary libraries
library(bestNormalize)
library(dplyr)
library(GGally)
library(ggplot2)
library(lmtest)
library(plm)

data <- read.csv("Data/data_no_missings.csv", header = T)

# needed ?
data$X <- NULL

# shortening names of the columns
colnames(data) <- c("Country", "Year", "Homicide", "Inequality", "Education_years", "GDP_per_capita",
                    "Lower_secondary_completion_rate", "RnD_expenditure", "School_enrollment", 
                    "Unemployment", "Urbanization_rate","Unsentenced", "Police")

summary(data)

# Huge plot, but may be interesting (takes some time)
ggpairs(data[,3:13], lower = list(combo = "box"), upper = list(combo = "blank"))


# Homicide transformation ----------------------------------------------------

ggplot(data) +
  geom_histogram(aes(Homicide), fill = "Steelblue", color = "black")

any(data$Homicide <= 0)
# Thus, we do not need to add anything to Homicide in log-transformation

ggplot(data) +
  geom_histogram(aes(log(Homicide)), fill = "Steelblue", color = "black")

# Box-Cox rather indicates log-transformation
bestNormalize::boxcox(data$Homicide)$lambda

# Therefore, we create the logarithm of the dependent variable
data$ln_Homicide <- log(data$Homicide)


# We can apply Box-Cox transformation also for some other variables 
# and check whether any transformation should be considered.

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


# Great plot to show how Homicide changed across years (easily can be changed with countries)
library(gplots)

plotmeans(ln_Homicide ~ Year, main="Heterogeineity across years", data=data)


# Basic Reproduction -----------------------------------------------

# Choosing vars, without RnD_expenditure and Urbanization_rate, filtering appropriate years
df <- data %>%
  select(Country, Year, Homicide, Inequality, Education_years, 
         GDP_per_capita, Lower_secondary_completion_rate, 
         School_enrollment, Unemployment, Unsentenced, Police) %>%
  filter(Year %in% 2003:2015)


## Data transformation --------------------------------------

# Homicide
bestNormalize::boxcox(df$Homicide)$lambda

df$ln_Homicide <- log(df$Homicide)

# GDP per capita
bestNormalize::boxcox(df$GDP_per_capita)$lambda

df$ln_GDP_per_capita <- log(df$GDP_per_capita)

# Unemployment
# We transform it exactly in the same way it was done in the Bachelor thesis.
df$Unemployment_int <- cut(df$Unemployment,
                           breaks = c(0,5.5,8.5,Inf),
                           labels = c("low", "medium","high"))

table(df$Unemployment_int)
# Different frequency!! :O 


library(corrplot)

correlations <- df %>%
  select(ln_Homicide, Inequality, Education_years, ln_GDP_per_capita,
         Lower_secondary_completion_rate, School_enrollment, 
         Unemployment, Unsentenced, Police) %>%
  cor(use = "pairwise.complete.obs")

corrplot.mixed(correlations,
               upper = "number",
               lower = "circle",
               tl.col= "black",
               tl.pos = "lt")

# quite different, but more or less the same coefficients
# probably because of the data base change


### Model --------------------------------------------

# fixed effects model
fixed <- plm(ln_Homicide ~ Inequality + Education_years + ln_GDP_per_capita +
             Lower_secondary_completion_rate + School_enrollment + 
             Unemployment_int + Unsentenced + Police,
             data = df, 
             index=c("Country", "Year"),
             model="within")

summary(fixed)

# fixed, individual effects
fixef(fixed)

# random effects model
random <- plm(ln_Homicide ~ Inequality + Education_years + ln_GDP_per_capita +
               Lower_secondary_completion_rate + School_enrollment + 
               Unemployment_int + Unsentenced + Police,
             data = df, 
             index=c("Country", "Year"),
             model="random")

summary(random)

# POLS
pols <- plm(ln_Homicide ~ Inequality + Education_years + ln_GDP_per_capita +
              Lower_secondary_completion_rate + School_enrollment + 
              Unemployment_int + Unsentenced + Police,
            data = df,
            index=c("Country", "Year"),
            model="pooling")

# F test for individual effects for fixed model
pFtest(fixed, pols) 
# (checking whether fixed model is necessary at all;
# we reject the null, which means that individual effects are significant)

# and individual effects for random effects
plmtest(pols, type=c("bp"))
# also effects are significant

# Testing time-fixed effects. The null is that no time-fixed effects needed
plmtest(fixed, c("time"), type=("bp"))
# Fail to reject the null -> No need to use time-fixed effects

### This part probably should be presented later at the final model ###################

# Testing for cross-sectional dependence/contemporaneous correlation:
# using Breusch-Pagan LM test of independence and Pesaran CD test
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
bptest(ln_Homicide ~ Inequality + Education_years + ln_GDP_per_capita +
         Lower_secondary_completion_rate + School_enrollment + 
         Unemployment_int + Unsentenced + Police,
       data = df,
       studentize=F)
#########################################################################################

# hausmann test
phtest(fixed, random)
# reject the null -> we choose fixed effects model


# Fixed model preparation  -------------------------------------------------

library(aod)

# deleting insignificant variables with 'general-to-specific' procedure

# Base model
model <- plm(ln_Homicide ~ Inequality + Education_years + ln_GDP_per_capita +
               Lower_secondary_completion_rate + School_enrollment + 
               Unemployment_int + Unsentenced + Police,
             data = df, 
             index=c("Country", "Year"),
             model="within")

summary(model)

## Lower_secondary_completion_rate

# We generate the model without Lower_secondary_completion_rate

model1 <- plm(ln_Homicide ~ Inequality + Education_years + ln_GDP_per_capita +
              School_enrollment + Unemployment_int + Unsentenced + Police,
              data = df, 
              index=c("Country", "Year"),
              model="within")


# check the significance of this change
h <- rbind(c(0,0,0,1,0,0,0,0,0))

wald.test(b = coef(model), Sigma = vcov(model), L = h)
# we fail to reject the null hypothesis -> no reason to omit this restriction 

summary(model1)

## Police

# Let us now check the whether the 'Police' coefficient is equal to zero.

# Therefore, we generate the model without Lower_secondary_completion_rate and without Police

model2 <- plm(ln_Homicide ~ Inequality + Education_years + ln_GDP_per_capita +
                School_enrollment + Unemployment_int + Unsentenced,
              data = df, 
              index=c("Country", "Year"),
              model="within")

h <- rbind(c(0,0,0,1,0,0,0,0,0),c(0,0,0,0,0,0,0,0,1))

wald.test(b = coef(model), Sigma = vcov(model), L = h)
# we fail to reject the null hypothesis that those two coefficients are equal to 0.

summary(model2)

## Inequality vs School_enrollment

# Based on literature Inequality ought to be more significant. 
# We check both approaches: either deleting Inequality variable or School_enrollment

## Inequality

# Now we generate the model without Lower_secondary_completion_rate, Police and Inequality

model3 <- plm(ln_Homicide ~ Education_years + ln_GDP_per_capita +
                School_enrollment + Unemployment_int + Unsentenced,
              data = df, 
              index=c("Country", "Year"),
              model="within")

h <- rbind(c(0,0,0,1,0,0,0,0,0),c(0,0,0,0,0,0,0,0,1),c(1,0,0,0,0,0,0,0,0))

wald.test(b = coef(model), Sigma = vcov(model), L = h)
# we fail to reject the null hypothesis that all those coefficients are equal to 0.

summary(model3)
# School_enrollment still significant

# School_enrollment

# Now we generate the model without Lower_secondary_completion_rate, Police and School_enrollment

model4 <- plm(ln_Homicide ~ Inequality + Education_years + ln_GDP_per_capita +
                Unemployment_int + Unsentenced,
              data = df, 
              index=c("Country", "Year"),
              model="within")

h <- rbind(c(0,0,0,1,0,0,0,0,0),c(0,0,0,0,0,0,0,0,1),c(0,0,0,0,1,0,0,0,0))

wald.test(b = coef(model), Sigma = vcov(model), L = h)
# we are close to reject the null hypothesis that all those coefficients are equal to 0.
# Therefore, we rather should not delete School_enrollment variable

summary(model4)
# Inequality still insignificant


# Our final model
summary(model3)

# It is different than the one in Bachelor Thesis...

# Let us check the one obtained there:
model_Bachelor <- plm(ln_Homicide ~ Inequality + ln_GDP_per_capita + School_enrollment +
                        Police + Unsentenced,
                      data = df, 
                      index=c("Country", "Year"),
                      model="within")

summary(model_Bachelor)
# Not all variables are significant in the model constructed in this way.


# Model diagnostic -------------------------------------------------------

# RESET test for plm() model??

# test Jarque-Bera - test na normalność rozkładu błędów losowych ??


# Testing for cross-sectional dependence/contemporaneous correlation:
# using Breusch-Pagan LM test of independence and Pesaran CD test
pcdtest(model3, test = c("lm"))
pcdtest(model3, test = c("cd"))
# we reject the null -> we have a cross-sectional correlation

# Testing for serial correlation for fixed model
pbgtest(model3)
# there is serial correlation
# we have to do some time series modeling ?


# Testing for heteroskedasticity
bptest(ln_Homicide ~ Education_years + ln_GDP_per_capita +
         School_enrollment + Unemployment_int + Unsentenced,
       data = df,
       studentize=F)
# The null hypothesis for the Breusch-Pagan test is homoskedasticity.


# Estimators ? ------------------------------------------------------

# heteroskedasticity-robust estimator for fixed-effects model
coeftest(model3, vcov.=vcovHC(model3, type="HC0", cluster="group")) # Heteroskedasticity consistent coefficients

# autocorrelation-robust estimator for fixed-effects model
coeftest(model3, vcov.=vcovNW(model3, type="HC0", cluster="group"))

# robust for cross-sectional and serial correlation estimator
coeftest(model3, vcov.=vcovSCC(model3, type="HC0", cluster="time"))

# "arellano" - both heteroskedasticity and serial correlation (recommended for fixed effects)
coeftest(fixed, vcovHC(model3, method = "arellano"))


