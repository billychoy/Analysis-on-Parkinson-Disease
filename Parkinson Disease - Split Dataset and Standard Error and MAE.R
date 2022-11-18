library(tidyverse)
library(modelr)#provides helper functions for computing regression model performance metrics
library(broom)#creates easily a tidy data frame containing the model statistical metrics

rm(list=ls())

# Load the data
data("swiss")

# Inspect the data
sample_n(swiss, 3)

#Model 1, including all predictors
model1 <- lm(Fertility ~., data = swiss)
summary(model1)
AIC(model1)
BIC(model1)

library(modelr)
data.frame(
  R2 = rsquare(model1, data = swiss),
  RMSE = rmse(model1, data = swiss),
  MAE = mae(model1, data = swiss)
)

library(caret)
library(plyr)
predictions <- model1 %>% predict(swiss)
data.frame(
  R2 = R2(predictions, swiss$Fertility),
  RMSE = RMSE(predictions, swiss$Fertility),
  MAE = MAE(predictions, swiss$Fertility)
)

library(broom)
glance(model1)

#Model 2, including all predictors except the variable Examination
model2 <- lm(Fertility ~. -Examination, data = swiss)
# Make predictions and compute the
# R2, RMSE and MAE
swiss %>%
  add_predictions(model2) %>%
  summarise(
    R2 = cor(Fertility, pred)^2,
    MSE = mean((Fertility - pred)^2),
    RMSE = sqrt(MSE),
    MAE = mean(abs(Fertility - pred))
  )
glance(model2)


#The two models have exactly the samed adjusted R2 (0.67), meaning that they are equivalent in explaining the outcome. 
Additionally, they have the same amount of residual standard error (RSE or sigma = 7.17). 
However, the model 2 is more simple than model 1 because it incorporates less variables. All things equal, 
the simple model is always better in statistics.

#The AIC and the BIC of the model 2 are lower than those of the model1. In model comparison strategies, the model with the lowest AIC and BIC score is preferred.



# RMSE and the RSE are measured in the same scale as the outcome variable
#Dividing the RSE by the average value of the outcome variable will give you the prediction error rate, which should be as small as possible
sigma(model1)/mean(swiss$Fertility)
sigma(model2)/mean(swiss$Fertility)
#In our example the average prediction error rate is 10%.
