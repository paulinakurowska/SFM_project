# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()
setwd("C:/Users/ThinkPad/Desktop/TeXBeamer")
# install and load packages
if (!require("fGarch")) {
  install.packages("fGarch")
}
library(fGarch)


#set seed
set.seed(134)

#set n for n=1000
sim1000 = garchSim(spec = garchSpec(model = list(alpha = 0.9, beta = 0)),1000)
#plot PACF of squared Observations
sim10002 = sim1000^2
pacf(sim10002)


#ARCH model estimation
model1000  =  garchFit(~garch(1), data =sim1000,cond.dist = "QMLE")
#plot of Time Series
plot(sim1000@.Data,type="l",xlab="", ylab="",ylim=c(-0.02,0.02))
#Quantile-Quantile Plot of Standardized Residuals
res=model1000@residuals
sig=(model1000@sigma.t)^2
std_res=res/sig/1000
length(std_res)
qqplot(qnorm((1:1000-0.5)/1000),sort(std_res),ylim=c(-3,3),xlim=c(-3,3),xlab="Theoretical Quantiles",ylab="Sample Quantiles")
abline(mean(std_res), sd(std_res), col="blue", lwd=2)


