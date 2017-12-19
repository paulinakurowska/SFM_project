# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
if(!require("fGarch")){install.packages("fGarch")};  library(fGarch)
if(!require("grid")){install.packages("grid")};  library(grid)
if(!require("gridExtra")){install.packages("gridExtra")};  library(gridExtra)

#set seed
set.seed(134)
#simulated ARCH(1) model with n=100 with alpha=0.9 
sim100<-garchSim(spec = garchSpec(model = list(alpha = 0.9, beta = 0)),100)
#plot PACF of squared Observations
sim1002<-sim100^2
pacf(sim1002)
#ARCH model estimation
model100 <- garchFit(~garch(1), data =sim100,cond.dist = "QMLE")
summary(model100)
#plot of Time Series
plot(model100,which = 1)
#plot of Standardized Residuals
plot(model100,which = 9)
#Quantile-Quantile Plot of Standardized Residuals
plot(model100,which = 13)

#now the same for n=1000
sim1000<-garchSim(spec = garchSpec(model = list(alpha = 0.9, beta = 0)),1000)
#plot PACF of squared Observations
sim10002<-sim1000^2
pacf(sim10002)
#ARCH model estimation
model1000 <- garchFit(~garch(1), data =sim1000,cond.dist = "QMLE")
#plot of Time Series
plot(model1000,which = 1)
#plot of Standardized Residuals
plot(model1000,which = 9)
#Quantile-Quantile Plot of Standardized Residuals
plot(model1000,which = 13)

                                  
