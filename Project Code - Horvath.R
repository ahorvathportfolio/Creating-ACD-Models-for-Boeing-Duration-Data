# Creating ACD Models for High Frequency Data - Andras Horvath
# Time Series Project

setwd("Your directory to folder containing Boeing.csv")
library(ACDm)

######################
### Preparing Data ###
######################
Bt=read.csv("Boeing.csv")

head(Bt, 5)
#year month day hour minute second price volume time
#1 2008 12 1 9 30  2 41.64 3441 2008-12-01 09:30:02
#2 2008 12 1 9 30  2 41.64 3441 2008-12-01 09:30:02
#3 2008 12 1 9 30 12 41.43 100  2008-12-01 09:30:12
#4 2008 12 1 9 30 16 41.82 100  2008-12-01 09:30:16
#5 2008 12 1 9 30 16 41.83 100  2008-12-01 09:30:16

# We want the durations:
dur=computeDurations(Bt, open = "09:30:00", close = "16:00:00",
                     rm0dur = TRUE, type = "trade")

# We only want to extract positive durations (setting rm0dur = TRUE removes 
# the zero durations).

head(dur, 5)
#                 time    price volume Ntrans durations
#1 2008-12-01 09:30:02 41.64000   6882      2         2
#2 2008-12-01 09:30:12 41.43000    100      1        10
#3 2008-12-01 09:30:16 41.91116   2016     11         4
#4 2008-12-01 09:30:17 41.97000   1000      3         1
#5 2008-12-01 09:30:27 41.50500    200      2        10

################
### Analysis ###
################

# a) Plot the data in meaningful ways and describe its salient features. Is 
# there a diurnal cycle/trend?
ts.plot(dur$durations, ylab = "Durations", main = "Time Plot of Durations", xlab = "Time Index") 
# There is definitely a pattern going on here. Let's break it up day by day:
# Entries 1-10274 is day one
# Up to 20432 is day 2
# Up to 31708 is day 3
# Up to 41968 is day 4
abline(v = 10274, col = "red", lty = 2)
abline(v = 20432, col = "red", lty = 2)
abline(v = 31708, col = "red", lty = 2)
abline(v = 41968, col = "red", lty = 2)

# ACF plot:
acf(dur$durations, main = "ACF of Durations")
# Not decaying and also outside of bands. This confirms we have problems. 


# b) If there is a cycle and/or other systematic trends, you will need to model 
# it/them and extract the residuals before proceeding with ACD modeling. One 
# possibility is to construct your own trend functions as on p. 253 of the AFTS 
# book. Another option is to use the diurnalAdj function in ACDm to perform 
# nonparametric smoothing. If you use the latter, note that the aggregate option 
# is day specific, e.g., if “all” then all days are smoothed in the same way, if 
# ”none” then each day is smoothed differently, etc.

help("diurnalAdj") # Performs nonparametric smoothing
# Note: aggregate option is day specific, e.g., if “all” then all days are
# smoothed in the same way, if ”none” then each day is smoothed differently, etc.

# Nodes:
#> 54795-41968
#[1] 12827
#> 41968-31708
#[1] 10260
#> 31708-20432
#[1] 11276
#> 20432-10274
#[1] 10158
#> 10274-0
#[1] 10274
#9*60+30 #570 min after midnight
#16*60 #960 min after midnight
#nodes=c(seq(570,960,60),960)

############
### Note ###
############
# Play around with the interval for the nodes i.e. 5 minutes, 15 minutes, etc. intervals.

data_new1 <- diurnalAdj(dur = dur, method = "cubicSpline", aggregation = "all", nodes = c(seq(570,960,60),960))
ts.plot(data_new1$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new1$adjDur, main = "ACF of Durations") # NOT GOOD

par(mfrow = c(1,1))
data_new2 <- diurnalAdj(dur = dur, method = "supsmu", aggregation = "all", span = "cv")
ts.plot(data_new2$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new2$adjDur, main = "ACF of Durations") # NOTE GOOD

data_new3 <- diurnalAdj(dur = dur, method = "smoothSpline", aggregation = "all", nodes = c(seq(570,960,15),960), spar = 0.02)
ts.plot(data_new3$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new3$adjDur, main = "ACF of Durations") # NOT GOOD, NO MATTER WHAT I DO TO CHANGE THINGS

data_new4 <- diurnalAdj(dur = dur, method = "FFF", aggregation = "all", Q = 4)
ts.plot(data_new4$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new4$adjDur, main = "ACF of Durations") # NOT GOOD no matter what Q is.

###

data_new5 <- diurnalAdj(dur = dur, method = "cubicSpline", aggregation = "none", nodes = c(seq(570,960,5),960)) # 5 or 10 minutes best
ts.plot(data_new5$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new5$adjDur, main = "ACF of Durations") # Pretty darn good using 5 minute intervals!!

data_new6 <- diurnalAdj(dur = dur, method = "supsmu", aggregation = "none", span = "cv")
ts.plot(data_new6$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new6$adjDur, main = "ACF of Durations") # NOT GOOD

#Choose the one below
data_new7 <- diurnalAdj(dur = dur, method = "smoothSpline", aggregation = "none", nodes = c(seq(570,960,5),960), spar = 0)
ts.plot(data_new7$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new7$adjDur, main = "ACF of Durations") # Pretty darn good using 5 minute intervals and default spar = 0 (spar in 0 to 1 possible)

data_new8 <- diurnalAdj(dur = dur, method = "FFF", aggregation = "none", Q = 50)
ts.plot(data_new8$adjDur, ylab = "Durations", main = "Time Plot of Durations") 
acf(data_new8$adjDur, main = "ACF of Durations") # Pretty darn good, but Q is way bigger then the default Q = 4


# RESULT: USE EITHER THE CUBIC SPLINE data_new_5 or SMOOTH SPLINE data_new_7. BOTH SEEM EQUALLY
# GOOD based on the ACF plot. 
data_new = data_new7 # Looking back on this project now I seem to recall trying both
# out and came to the decision then that the SMOOTH SPLINE was the way to go.

# c) Fit ACD models using the function acdFit. Explore the range of available models 
# (see help file), and decide on what you believe to be the “best” model. Note that 
# you have several “plot” functions and other diagnostics to help you answer this 
# question.

# Build a duration model for the adjusted series using exponential innovations. 
# Check the fitted model.
m1 <- acdFit(durations = data_new, model = "ACD", dist = "exponential")
acf(m1$residuals, main = "EACD(1,1) ACF of Residuals")
acf(m1$residuals^2, main = "EACD(1,1) ACF of Residuals^2") # Both ACFs look great

Box.test(m1$residuals, lag = 10, "Ljung") 
Box.test(m1$residuals^2, lag = 10, "Ljung")
Box.test(m1$residuals, lag = 20, "Ljung") 
Box.test(m1$residuals^2, lag = 20, "Ljung")


# Build a duration model for the adjusted series using Weibull innovations. Check 
# the fitted model.
m2 <- acdFit(durations = data_new, model = "ACD", dist = "weibull")
acf(m2$residuals, main = "WACD(1,1) ACF of Residuals")
acf(m2$residuals^2, main = "WACD(1,1) ACF of Residuals^2") # Both ACFs look great

Box.test(m2$residuals, lag = 10, "Ljung") 
Box.test(m2$residuals^2, lag = 10, "Ljung") 
# Everything great! Up to alpha = 0.05 level that is.
Box.test(m2$residuals, lag = 20, "Ljung") 
Box.test(m2$residuals^2, lag = 20, "Ljung") 

# Build a duration model for the adjusted series using generalized gamma innovations. 
# Check the fitted model.
m3 <- acdFit(durations = data_new, model = "ACD", dist = "gengamma")
acf(m3$residuals)
acf(m3$residuals^2) # Both look great

Box.test(m3$residuals, lag = 10, "Ljung") 
Box.test(m3$residuals^2, lag = 10, "Ljung")
# Not good. The Ljung-Box tests fail at alpha = 0.05. Gengamma not a good option.

################################################################################
m4 <- acdFit(durations = data_new, model = "ACD", dist = "burr")
acf(m4$residuals)
acf(m4$residuals^2)

Box.test(m4$residuals, lag = 10, "Ljung") 
Box.test(m4$residuals^2, lag = 10, "Ljung")
# Fails Ljung miserably.

m5 <- acdFit(durations = data_new, model = "ACD", dist = "genf")
acf(m5$residuals)
acf(m5$residuals^2)

Box.test(m5$residuals, lag = 10, "Ljung") 
Box.test(m5$residuals^2, lag = 10, "Ljung")
# Fails Ljung.

m6 <- acdFit(durations = data_new, model = "ACD", dist = "qweibull")
#acf(m6$residuals)
#acf(m6$residuals^2)

#Box.test(m6$residuals, lag = 10, "Ljung") 
#Box.test(m6$residuals^2, lag = 10, "Ljung")
# Error. Optimization Problems...

m7 <- acdFit(durations = data_new, model = "ACD", dist = "mixqwe")
#acf(m7$residuals)
#acf(m7$residuals^2)

#Box.test(m7$residuals, lag = 10, "Ljung") 
#Box.test(m7$residuals^2, lag = 10, "Ljung")
# Error. Optimization Problems...

m8 <- acdFit(durations = data_new, model = "ACD", dist = "mixqww")
#acf(m8$residuals)
#acf(m8$residuals^2)

#Box.test(m8$residuals, lag = 10, "Ljung") 
#Box.test(m8$residuals^2, lag = 10, "Ljung")
# Error. Optimization Problems...

m9 <- acdFit(durations = data_new, model = "ACD", dist = "mixinvgauss")
#acf(m9$residuals)
#acf(m9$residuals^2)

#Box.test(m9$residuals, lag = 10, "Ljung") 
#Box.test(m9$residuals^2, lag = 10, "Ljung")
# Error. Optimization Problems...

################################################################################
# So assumed error term distributions that work are Exponential and Weibull. Note:
# these are EACD(1,1) and WACD(1,1) by default as order=c(1,1) is used.

# Compare and comment on the duration models built before:
# Find the lowest AIC to see which model is preferred.
m1$goodnessOfFit # Exponential
#                      value
#LogLikelihood -5.455432e+04
#AIC            1.091146e+05
#BIC            1.091414e+05
#MSE            7.048921e-01

m2$goodnessOfFit # Weibull
#                      value
#LogLikelihood -4.919128e+04
#AIC            9.839056e+04
#BIC            9.842621e+04
#MSE            7.051147e-01

# Lowest AIC and BIC belong to the Weibull. m2 is the way to go!


# d) Is it possible to do forecasting with your model? Explore this and attempt 
# to forecast the next 3 durations. (You may need to consult the literature...)

# Look at acdFit-methods in the ACDm package.
residuals(m2)
coef(m2)
print(m2)
# These all work, but
predict(m2) # does not work.
predict(m2, N = 3) # it gives an error that the ACD model should have 3 parameters,
# but I do not know what to put in place of the ...
predict(m2, n.ahead = 3)
predict(m1, n.ahead = 3) # Does have predictions.

# There are no forecasting options in the file.
# There is a package racd but it does not have any rcran documentation and I am
# unable to install the racd package.

# So on this front I have to unfortunately come up short.
# However, I am not too discouraged looking back at this project as Dr. Trindade
# said he did not know of any forecasting options either at that time.

################################################################################
################################################################################
#Let's keep messing around and seeing if we can get a better model than the 
# WACD(1,1) where AIC = 9.839056e+04 and BIC = 9.842621e+04
data_new2 = data_new8

# Build a duration model for the adjusted series using Weibull innovations. Check 
# the fitted model.
m1new <- acdFit(durations = data_new, model = "ACD", dist = "weibull", order = c(2,2))
acf(m1new$residuals)
acf(m1new$residuals^2)

Box.test(m1new$residuals, lag = 10, "Ljung") 
Box.test(m1new$residuals^2, lag = 10, "Ljung") 
# Everything great! Up to alpha = 0.05 level that is.
m1new$goodnessOfFit # Weibull(2,2)
# LogLikelihood -4.919132e+04
# AIC            9.839464e+04
# BIC            9.844811e+04
# MSE            7.051233e-01
# The WACD(2,2) is not better.

# New data:
m2new <- acdFit(durations = data_new2, model = "ACD", dist = "exponential")
acf(m2new$residuals)
acf(m2new$residuals^2)

Box.test(m2new$residuals, lag = 10, "Ljung") 
Box.test(m2new$residuals^2, lag = 10, "Ljung") 
# Everything great! Up to alpha = 0.05 level that is.
m2new$goodnessOfFit
#LogLikelihood -5.477896e+04
#AIC            1.095639e+05
#BIC            1.095907e+05
#MSE            6.955916e-01
# Not better

m3new <- acdFit(durations = data_new2, model = "ACD", dist = "weibull")
acf(m3new$residuals)
acf(m3new$residuals^2)

Box.test(m3new$residuals, lag = 10, "Ljung") 
Box.test(m3new$residuals^2, lag = 10, "Ljung") 
# Everything great! Up to alpha = 0.05 level that is.
m3new$goodnessOfFit
#LogLikelihood -4.926981e+04
#AIC            9.854761e+04
#BIC            9.858326e+04
#MSE            6.957844e-01
# Not better

m4new <- acdFit(durations = data_new2, model = "ACD", dist = "gengamma")
acf(m4new$residuals)
acf(m4new$residuals^2)

Box.test(m4new$residuals, lag = 10, "Ljung") 
Box.test(m4new$residuals^2, lag = 10, "Ljung") 
# Does not pass at 0.05 level


# Conclusion: WACD(1,1) is the best model
#Parameter estimate:
#        Coef      SE PV
#omega  0.2946 0.03017  0
#alpha1 0.0537 0.00375  0
#beta1  0.6553 0.03201  0
#gamma  1.4051 0.00413  0

#fitmodel = acdFit(adjDurData)


# Post project feedback: The only major thing that I would need to reevaluate are the
# diurnal pattern estimates plots for the "best" models as mine are not very smooth 
# at all. This suggests that my best ACD models may in fact not actually be the best.
# As such one critique is to go back and continue investigating until an adequately 
# smooth diurnal pattern estimate plot is produced by diurnalAdj and then proceed
# as was done in the project.
