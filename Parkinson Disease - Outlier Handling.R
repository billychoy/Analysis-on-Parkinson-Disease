rm(list=ls())

library(tidyverse)

# Read in the Lung Cap Data
#read.table(file.choose(), header=T)
pks <- read.table(file.choose(" "), header=T, sep=",")  

# Attach pks_filter
attach(pks)
head(pks)

install.packages('psych')

library('psych')
describe(pks)

typeof(pks$total_UPDRS)
typeof(pks$age)
typeof(pks$sex)
typeof(pks$test_time)
typeof(pks$Shimmer)
typeof(pks$Jitter)
typeof(pks$subject)

#-------------- Check QQ Plot and line --------------------
#install.packages('qqplotr')
library('qqplotr') 
#qqnorm(): produces a normal QQ plot of the variable
#qqline(): adds a reference line

qqnorm(pks$age, pch = 1, frame = FALSE)
qqline(pks$age, col = "steelblue", lwd = 2)

qqnorm(pks$sex, pch = 1, frame = FALSE)
qqline(pks$sex, col = "steelblue", lwd = 2)

qqnorm(pks$test_time, pch = 1, frame = FALSE)
qqline(pks$test_time, col = "steelblue", lwd = 2)

qqnorm(pks$Shimmer, pch = 1, frame = FALSE)
qqline(pks$Shimmer, col = "steelblue", lwd = 2)

qqnorm(pks$Jitter, pch = 1, frame = FALSE)
qqline(pks$Jitter, col = "steelblue", lwd = 2)

qqnorm(pks$subject, pch = 1, frame = FALSE)
qqline(pks$subject, col = "steelblue", lwd = 2) 


#------- Define multivariate Regression
library(car)
reg <- lm(total_UPDRS ~ age+sex+test_time+Shimmer+Jitter+subject, data=pks)
summary(reg)
 

#Outliner test
outlierTest(reg)

qqPlot(reg,labels=row.names(pks), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

reg_factor <- lm(total_UPDRS ~ age+as.factor(sex)+test_time+Shimmer+Jitter+as.factor(subject), data=pks)
summary(reg_factor)


#Outliner test
outlierTest(reg_factor)

qqPlot(reg_factor,labels=row.names(pks), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")


######### MAE #################
install.packages("Metrics")
library("Metrics")

mae(pks$total_UPDRS, predict(reg))
mae(pks$total_UPDRS, predict(reg_factor))

install.packages("Metrics")
library("Metrics")

######### RMSE#################
rmse(pks$total_UPDRS, predict(reg))
rmse(pks$total_UPDRS, predict(reg_factor))

# Compute the analysis of variance
aov(reg_factor, data = pks)
# Summary of the analysis
summary(res.aov)

 
library("STAT")
var.test(reg,reg_factor, alternative = "two.sided") 

var.test(reg_factor,reg, alternative = "two.sided") 