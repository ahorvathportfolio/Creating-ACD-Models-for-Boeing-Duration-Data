# Creating-ACD-Models-for-Boeing-Duration-Data
 The goal of this project was to analyze the durations data for Boeing stock trades (5-day period spanning from December 1st to 5th
 in 2008), and to find the best autoregressive conditional duration (ACD) model, originally proposed by Engle & Russell (1998), using the R package ACDm.
 This was done by:
 - Checking for diurnal cycles/trends
 - Modeling these trends, if they exist, and using a nonparamteric smoother before extracting the residuals
 - Fitting ACD models and comparing the AIC and BIC of the "best" models to find the overall "winning" model
 - Finally, if it was possible, performing out of sample forecasting
