rm(list=ls())

library(tidyverse)

# Read in the Lung Cap Data
#read.table(file.choose(), header=T)
pks <- read.table(file.choose(" "), header=T, sep=",")
#Filter out some data field is not fit for the model, Nos, Test_time_hr, test)time_min
pks_filter <- pks[ -c(1:2,6:7) ]

#install.packages("PerformanceAnalytics")
#library(PerformanceAnalytics)
#chart.Correlation(pks_filter, histogram=TRUE, pch=19)

# Attach pks_filter
attach(pks_filter)
head(pks_filter)

# stepwise forward regression
library(olsrr)
model.fwd <- lm(total_UPDRS ~ ., data = pks_filter)
summary(model.fwd)
ols_step_forward_p(model.fwd, details = FALSE)

#Stepwise AIC Forward Regression
#Build regression model from a set of candidate predictor variables by entering predictors based on Akaike Information Criteria 
ols_step_forward_aic(model.fwd, details = TRUE)

# stepwise forward regression
library(olsrr)
model.bkw <- lm(total_UPDRS ~ ., data = pks_filter)
summary(model.bkw)
ols_step_backward_p(model.bkw, details = TRUE)

#Build regression model from a set of candidate predictor variables by entering predictors based on Akaike Information Criteria,
ols_step_backward_aic(model.bkw, details = TRUE)


#stepwise regression
#Build regression model from a set of candidate predictor variables by entering and removing predictors based on p values, in a stepwise manner until there is no variable left to enter or remove any more. The model should include all the candidate predictor variables. If details is set to TRUE, each step is displayed.
model.stpw <- lm(total_UPDRS ~ ., data = pks_filter)
ols_step_both_p(model.stpw, details = TRUE)

#Stepwise AIC Regression
#Build regression model from a set of candidate predictor variables by entering and removing predictors based on Akaike Information Criteria, in a stepwise manner until there is no variable left to enter or remove any more. The model should include all the candidate predictor variables. If details is set to TRUE, each step is displayed.
ols_step_both_aic(model.stpw, details = TRUE)

model.stpwf <- lm(total_UPDRS ~ ., data = pks)
ols_step_both_aic(model.stpwf, details = TRUE)

ols_step_backward_aic(model.stpwf, details = TRUE)

 
#Select the subset of predictors that do the best at meeting some well-defined objective criterion, such as having the largest R2 value or the smallest MSE, Mallow’s Cp or AIC.
model.all <- lm(total_UPDRS ~ ., data = pks_filter)
ols_step_best_subset(model.all, details = TRUE)

#install.packages("regclass")
library(regclass)

model.one <- lm(total_UPDRS ~ ., data = pks)
VIF(model.one)

model.two <- lm(total_UPDRS ~ ., data = pks_filter)
VIF(model.two)

options(scipen = 999)
print(VIF(model.two),digits=2) 

head(pks_filter)

pks_VIF <- pks_filter[ -c(8,10,13,16) ]
head(pks_VIF)

model.three <- lm(total_UPDRS ~ ., data = pks_VIF)
VIF(model.three)
options(scipen = 999)
print(VIF(model.three),digits=2) 
 
ols_step_both_p(model.three, details = TRUE)