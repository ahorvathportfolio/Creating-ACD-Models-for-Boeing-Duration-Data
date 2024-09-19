# Creating ACD Models for High Frequency Data - Andras Horvath
# Time Series Project
# Presentation/Simplified Code

setwd("Your directory to folder containing Boeing.csv")
library(ACDm)

#Preparing data:
Bt=read.csv("Boeing.csv")
head(Bt, 5)
dur=computeDurations(Bt, open = "09:30:00", close = "16:00:00", rm0dur = TRUE, type = "trade")
head(dur, 5)

# Analysis of diurnal cycle
ts.plot(dur$durations, ylab = "Durations", main = "Time Plot of Durations", xlab = "Time Index") 
abline(v = 10274, col = "red", lty = 2)
abline(v = 20432, col = "red", lty = 2)
abline(v = 31708, col = "red", lty = 2)
abline(v = 41968, col = "red", lty = 2)
acf(dur$durations, main = "ACF of Durations")

# Nonparametric Smoothing
data_new2 <- diurnalAdj(dur = dur, method = "supsmu", aggregation = "all", span = "cv")
# This plot is actually quite decent. We want a smooth behavior.
ts.plot(data_new2$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new2$adjDur, main = "ACF of Durations")

data_new5 <- diurnalAdj(dur = dur, method = "cubicSpline", aggregation = "none", nodes = c(seq(570,960,5),960)) # 5 minutes best
ts.plot(data_new5$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new5$adjDur, main = "ACF of Durations")

data_new7 <- diurnalAdj(dur = dur, method = "smoothSpline", aggregation = "none", nodes = c(seq(570,960,5),960), spar = 0)
ts.plot(data_new7$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new7$adjDur, main = "ACF of Durations")

data_new8 <- diurnalAdj(dur = dur, method = "FFF", aggregation = "none", Q = 50)
ts.plot(data_new8$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new8$adjDur, main = "ACF of Durations")

data_new = data_new7

# Fit ACD models
m1 <- acdFit(durations = data_new, model = "ACD", dist = "exponential")
acf(m1$residuals, main = "EACD(1,1) ACF of Residuals")
acf(m1$residuals^2, main = "EACD(1,1) ACF of Residuals^2")
Box.test(m1$residuals, lag = 10, "Ljung") 
Box.test(m1$residuals^2, lag = 10, "Ljung")
Box.test(m1$residuals, lag = 20, "Ljung") 
Box.test(m1$residuals^2, lag = 20, "Ljung")

m2 <- acdFit(durations = data_new, model = "ACD", dist = "weibull")
acf(m2$residuals, main = "WACD(1,1) ACF of Residuals")
acf(m2$residuals^2, main = "WACD(1,1) ACF of Residuals^2")
Box.test(m2$residuals, lag = 10, "Ljung") 
Box.test(m2$residuals^2, lag = 10, "Ljung") 
Box.test(m2$residuals, lag = 20, "Ljung") 
Box.test(m2$residuals^2, lag = 20, "Ljung")

# Best model
m1$goodnessOfFit
m2$goodnessOfFit

# Post project feedback: The only major thing that I would need to reevaluate are the
# diurnal pattern estimates plots for the "best" models as mine are not very smooth 
# at all. This suggests that my best ACD models may in fact not actually be the best.
# As such one critique is to go back and continue investigating until an adequately 
# smooth diurnal pattern estimate plot is produced by diurnalAdj and then proceed
# as was done in the project.
