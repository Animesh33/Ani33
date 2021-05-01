library(tidyverse)
library(fpp2)
library(imputeTS)
library(na.tools)
library(nortest)
library(quantmod)
library(forecast)

#Problem 1
pepsi <- getSymbols("PEP", src="yahoo", auto.assign=F, from="2018-01-01", to="2020-02-29")
pepsi$ID <- 1:542
nestle <- getSymbols("NSRGY",src="yahoo", auto.assign=F, from="2018-01-01", to="2020-02-29")
delmonte <- getSymbols("D03.SI",src="yahoo", auto.assign=F, from="2018-01-01", to="2020-02-29")
oceanspray <- getSymbols("OCESO",src="yahoo", auto.assign=F, from="2018-01-01", to="2020-02-29")
coke <- getSymbols("KO",src="yahoo", auto.assign=F, from="2018-01-01", to="2020-02-29")
pepTS <- ts(pepsi$PEP.Close, start=c(2018, 1, 1), frequency=252)
pepTS<-na_remove(pepTS)

fit_ES <- ses(pepTS, alpha = 0.75)
summary(fit_ES)

#Problem 2

fit_AES <- holt(pepTS, alpha = 0.75, beta = 0.85)
summary(fit_AES)

#Problem 3

linearMod <- lm(data = pepsi , PEP.Close~ID)
predict(linearMod , newdata = data.frame(ID=seq(544,553)))
summary(linearMod)
RSS <- c(crossprod(linearMod$residuals))
MSE <- RSS / length(linearMod$residuals)
RMSE <- sqrt(MSE)
# Adjusted R sq. is Coeff. of Determination = 0.7605
coeff_cor <- sqrt(summary(linearMod)$r.squared)
coeff_deter <- summary(linearMod)$r.squared
hist(linearMod$residuals)
pearson.test(linearMod$residuals)
qqnorm(linearMod$residuals)
qqline(linearMod$residuals)
plot(linearMod$residuals , col = 'dark red')
plot(linearMod , col = 'lavender')

plot(linearMod$fitted.values , linearMod$residuals)

#Problem 4

collection <- cbind(pepsi,nestle,oceanspray,coke,delmonte)
model <- lm(data=collection, PEP.Close ~ NSRGY.Close + D03.SI.Close + KO.Close + OCESO.Close)
summary(model)
collection$PEP.Close_0 <- lag(collection$PEP.Close, 0)
collection$PEP.Close_1 <- lag(collection$PEP.Close, 1)
collection$PEP.Close_2 <- lag(collection$PEP.Close, 2)
collection$PEP.Close_3 <- lag(collection$PEP.Close, 3)
summary(lm(data=collection, PEP.Close_0 ~ PEP.Close_1 + PEP.Close_2 + PEP.Close_3))

collection$NSRGY.Close_0 <- lag(collection$NSRGY.Close, 0)
collection$NSRGY.Close_1 <- lag(collection$NSRGY.Close, 1)

collection$D03.SI.Close_0 <- lag(collection$D03.SI.Close, 0)
collection$D03.SI.Close_1 <- lag(collection$D03.SI.Close, 1)

collection$KO.Close_0 <- lag(collection$KO.Close, 0)
collection$KO.Close_1 <- lag(collection$KO.Close, 1)

finalMod <-lm(data=collection, PEP.Close_0 ~ PEP.Close_1 + NSRGY.Close_1 + D03.SI.Close_1 + KO.Close_1)
finalMod1 <- lm(data=collection , PEP.Close_0 ~ PEP.Close_1 + NSRGY.Close_1)
newData <- data.frame(PEP.Close_1 = 136.37 , NSRGY.Close_1 = 103.32)
summary(finalMod1)
predict(finalMod1 , newdata = newData)
RSS1 <- c(crossprod(finalMod$residuals))
MSE1 <- RSS1 / length(finalMod$residuals)
RMSE1 <- sqrt(MSE1)
