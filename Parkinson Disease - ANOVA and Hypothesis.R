rm(list=ls())

library(readr)
#-------------- Read file -------------------- 
pks <- read.table(file.choose(" "), header=T, sep=",")  
str(pks)
#-------------- Factor on subject --------------------
library("dplyr")
pks <- pks %>% mutate(subject = as.factor(subject))
str(pks)

#-------------- Histogram --------------------

hist(pks$total_UPDRS,main= "Histogram on total_UPDRS")

#-------------- Check QQ Plot and line --------------------
#install.packages('qqplotr')
library('qqplotr') 
#qqnorm(): produces a normal QQ plot of the variable
#qqline(): adds a reference line

qqnorm(pks$total_UPDRS, pch = 1, frame = FALSE)
qqline(pks$total_UPDRS, col = "steelblue", lwd = 2) 

mean(pks$total_UPDRS)
mean(pks$subject)

#-------------- anova test, total_UPDRS and subject --------------------
anova_updrs_subject <- aov(total_UPDRS~subject, data=pks)
summary(anova_updrs_subject)


#-------------- Age group--------------------
table(pks$age) 

pks$age_group <- pks$age
pks$age_group <- ifelse((pks$age>=0 & pks$age<=54) , 'below 55',pks$age_group)
pks$age_group <- ifelse((pks$age>54 & pks$age<=60) , '55-60',pks$age_group)
pks$age_group <- ifelse((pks$age>60 & pks$age<=65) , '61-65',pks$age_group)
pks$age_group <- ifelse((pks$age>65 & pks$age<=70) , '66-70',pks$age_group)
pks$age_group <- ifelse((pks$age>70 & pks$age<=75) , '71-75',pks$age_group)
pks$age_group <- ifelse((pks$age>75 & pks$age<=80) , '76-80',pks$age_group)
pks$age_group <- ifelse((pks$age>80) , '81 or above',pks$age_group)
pks$age_group<-as.factor(pks$age_group)
table(pks$age_group) 

table(pks$age_group) 

library("ggpubr")
library("ggplot2")
ggboxplot(pks,x="age_group",
          y="total_UPDRS", 
          color="age_group",
          palette ="Okabe-Ito",
          order=c("below 55","54-60","61-65","66-70", "71-75","76-80","81 or above"),
          ylab="total_UPDRS",xlab="Age Group")

str(pks$age_group)

#-------------- ANOVA test --------------------
anova_updrs_age <- aov(total_UPDRS~age_group, data=pks)
summary(anova_updrs_age)

#-------------- Homogeneity test --------------------
bartlett.test(total_UPDRS~age_group, data=pks) 
## 
##   Bartlett's Homogeneity Test (alpha = 0.05) 
## ----------------------------------------------- 
##   data : Sepal.Length and Species 
## 
##   statistic  : 462.65 
##   parameter  : 6 
##   p.value    : 2.2e-16 
## 
##   Result     : Variances are not homogeneous. 
## ----------------------------------------------- 


######## Nomarity test ################
library(nortest)
shapiro.test(pks$total_UPDRS ~pks$age_group)

library("ggpubr")
ggdensity(pks$age, 
          main = "Density plot of Age",
          xlab = "Age")
 
library(ggpubr)
ggqqplot(pks$age)
