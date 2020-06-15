library(survival)
library(tidyverse)
library(BeSS)
library(pec)
setwd("~/Downloads")
install.packages("C:/Users/akihi/Downloads/goftte_1.0.5.tar.gz", repos=NULL, type="source")
library(goftte)
#for-loop to do univariate cox models with all covariates
for (name in names(df)[!(names(df) %in% c("time", "status", "survobj"))]) {
  cat(name, "\n") #just for better layout, "\n" means "enter" (return in MAC)
  print(coxph(as.formula(paste("survobj ~", name, sep=" ")), data = df))
  cat("\n\n") #just for better layout
}
#then fit model with variables significant in univariate models

#model fit
PH = coxph(surbobj ~ age + gender, data = df)
Predicted.value=predict(PH)
Martingale.residuals=residuals(PH, type="martingale")
#Deviance residuals
#Deviance.residuals=residuals(PH, type="deviance")
plot(Prediccted.value, Martingale.residuals, type="p", pch=20)
abline(h=0, col="red", lty=2)

#functional form of predictors
#the observed curve should lie within the simulated curve if model fits
#continuous variables
fcov(PH, type.test="Lin")
plot(fcov(PH))
#for categorical variables
#plot log-cumulative hazard vs time or log(time)
KM0=survfit(survobj~1, data=df, subset=gender==0)
KM1=survfit(survobj~1, data=df, subset=gender==1)
#calculate logH(t)=log(-logS(t))
km0=data.frame(time=KM0$time,
               cloglogS=log(-log(KM0$surv+0.0001)))
km1=data.frame(time=KM1$time,
               cloglogS=log(-log(KM1$surv+0.0001)))
range(km0$time, km1$time)
range(km0$cloglogS, km1$cloglogS)
plot(km0$time, km0$cloglogS, type="s", col="blue", 
     xlab="time", ylab="log(-logS(t))", xlim=c(0,95), ylim=c(-3.5, 2.5))
lines(km1$time, km1$cloglogS, type="s", col="red")
#plot standardized score process
#continuous variables
# you can perform hypothesis testing
prop(PH, type.test="Lin") #default
prop(PH, type.test="Liu")
# you can also plot it
plot(prop(PH))

## Alternative: "ldatools" Package
#install.packages("devtools")
library(devtools)
#install_github("adibender/ldatools")
library(ldatools)
#Cox-Snell residuals
get_csvec(PH)
#cumulative hazard (Nelson-Aalen estimate) vs. Cox-Snell residuals
gg_coxsnell(PH, type="cumu_hazard")
#cumulative distribution function (Breslow estimate) vs. Cox-Snell residuals
gg_coxsnell(PH, type="cdf")

#scaled Schoenfeld residuals
get_scaledsch(PH, transform = "km")
#scaled schoenfeld residuals vs. (transformed) time.
gg_scaledsch(PH, transform = "km")

## Alternative: "survminer" Package
library(survminer)
# Test The Proportional Hazards Assumption Of A Cox Regression
ZPH=cox.zph(PH)
ZPH
# Graphical Test Of Proportional Hazards With ggplot2
# Displays a graph of the scaled Schoenfeld residuals, along with a smooth curve using ggplot2.
ggcoxzph(ZPH)
















