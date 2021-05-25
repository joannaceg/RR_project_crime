
# Setting working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading necessary libraries
library(bestNormalize)
library(dplyr)
library(GGally)
library(ggplot2)
library(plm)

data <- read.csv("Data/data_no_missings.csv", header = T, sep = ";")

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



