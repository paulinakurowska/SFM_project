# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
if(!require("fGarch")){install.packages("fGarch")};  library(fGarch)
if(!require("grid")){install.packages("grid")};  library(grid)
if(!require("gridExtra")){install.packages("gridExtra")};  library(gridExtra)

#set seed
set.seed(468)
#simulated ARCH(1) model with n=100 with alpha=0.9 
model100 <- garchFit(~garch(1), data = garchSim(spec = garchSpec(model = list(alpha = 0.9, beta = 0)), n=100),cond.dist = "QMLE")
model100
summary(model100)
#plot of Time Series
plot(model100,which = 1)
#plot ACF of Observations
plot(model100,which = 4)
#plot of Standardized Residuals
plot(model100,which = 9)
#Quantile-Quantile Plot of Standardized Residuals
plot(model100,which = 13)

#now the same for n=1000
model1000 <- garchFit(~garch(1), data = garchSim(spec = garchSpec(model = list(alpha = 0.9, beta = 0)), n=1000),cond.dist = "QMLE")
model1000
summary(model1000)
#plot of Time Series
plot(model1000,which = 1)
#plot ACF of Observations
plot(model1000,which = 4)
#plot of Standardized Residuals
plot(model1000,which = 9)
#Quantile-Quantile Plot of Standardized Residuals
plot(model1000,which = 13)
