# PS7
# Alli Penner

setwd("~/GitHub/DScourseS18")
library(mice)
library(MixedDataImpute)
library(stargazer)
library(VIM)

wage <- read.csv("Modeling/wages.csv")

# drop tenure and hgc missingness
wage <- wage[ !is.na(wage$hgc),]
wage <- wage[ !is.na(wage$tenure),]

stargazer(wage)

scattmatrixMiss(wage)

# Regression with imputations

# tenure^2
wage$tenure2 <- wage$tenure^2
# complete cases
wagecomplete <- wage[complete.cases(wage),]
CCreg <- lm(logwage~hgc+college+tenure+tenure2+age+married, wagecomplete)

# mean impute
wagemean <- wage
wagemean$logwage[is.na(wagemean$logwage)] <- mean(wage$logwage, na.rm = T)
meanreg <- lm(logwage~hgc+college+tenure+tenure2+age+married, wagemean)

# linear regress impute
wagelm <- wage
wagelm$logwage[is.na(wagelm$logwage)] <- predict(CCreg, 
                                                 wagelm[is.na(wagelm$logwage),])
lmreg <- lm(logwage~hgc+college+tenure+tenure2+age+married, wagelm)

#pmm  impute
temp <- mice(wage,m=5,maxit=50,meth='pmm',seed=500)
wagemice <- complete(temp,1)
micereg <- lm(logwage~hgc+college+tenure+tenure2+age+married, wagemice)

# table
stargazer(CCreg, meanreg, lmreg, 
          column.labels=c("Complete","Mean", "Linear"))




