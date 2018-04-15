###########################################################
# This is the Electricity Cutoff Data and Illustrates
# the use of offsets in Poisson regression
###########################################################
###########################################################
# Clear the workspace. I do this every time
###########################################################
rm(list = ls())
###########################################################
# Set the seed. I also do this every time
###########################################################
set.seed(4428967)
###########################################################
# Set the working directory. 
###########################################################
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Electricity_Cutoffs_Poisson")
###########################################################
# Load the libraries
###########################################################
library(mgcv)
library(HRW)
###########################################################
# Load the Data and get the names of the variables
###########################################################
cutoffs = read.csv("Disconnects_data.csv")
names(cutoffs)
###########################################################
# Simple Poisson Analysis, 2 splines
###########################################################
m4=gam(Disconnects~s(rolling_expirations)
        +s(rolling_enrollments)
        +Bills_Due+is_winter,data=cutoffs, family=poisson)
summary(m4)
thefit_Poisson = m4$fitted.values
###########################################################
# Simple Quasi-Poisson Analysis, 2 splines
###########################################################
mqpoi2=gam(Disconnects~s(rolling_expirations)
              +s(rolling_enrollments)
              +Bills_Due+is_winter,data=cutoffs, scale=-1, family=poisson)
summary(mqpoi2)
thefit_QuasiPoisson_2splines = mqpoi2$fitted.values

###########################################################
# Simple Quasi-Poisson Analysis, 1 spline
###########################################################
mqpoi3=gam(Disconnects~s(rolling_expirations)
            +rolling_enrollments
            +Bills_Due+is_winter,data=cutoffs, scale=-1, family=poisson)
anova(mqpoi2,mqpoi3, test="F")
thefit_QuasiPoisson_1splines = mqpoi3$fitted.values
summary(mqpoi3)



###########################################################
# Now add an offset. It makes sense that the number of bills 
# due has a direct impack on the number of cutoffs
#
# By the way, to add an offset, if you want Bills_Due
# as an offset, you should make it log(Bills_Due) within 
# the formula if you want better fitted values. However, 
# some Bills_Due = 0, so use log(Bills_Due+1)
#
# If you use the offset outside the formula, mgcv
# completely when getting the fitted values
###########################################################
#    Offset model:
offsetm1=gam(Disconnects~offset(log(Bills_Due+1))
  +s(rolling_expirations)
  +Bills_Due 
  +is_winter,data=cutoffs, family=quasipoisson)
###########################################################
# Notice that the summary has the offset in the model
# but with no statistical significance, as you wanted.
###########################################################
summary(offsetm1)
thefit_QuasiPoisson_1spline_Offset = offsetm1$fitted.values
###########################################################
# There is not much here except the intercept, but I keep
# the other 2 because it can happen that adding a couple 
# of not significant terms can lead to better predictions.
# AIC is for prediction, and it overfits slightly. 
#
# This is not a variable selection exercise
###########################################################

###########################################################
# Now do it without the spline
###########################################################
offsetm2=gam(Disconnects~offset(log(Bills_Due+1))
             +rolling_expirations
             +Bills_Due 
             +is_winter,data=cutoffs, family=quasipoisson)
thefit_QuasiPoisson_linear_Offset = offsetm1$fitted.values


thepredictions = cbind(cutoffs$Disconnects,thefit_Poisson,
                       thefit_QuasiPoisson_1splines,
                       thefit_QuasiPoisson_1spline_Offset,
                       thefit_QuasiPoisson_linear_Offset)
colnames(thepredictions) = c("Actual","Poisson","QuasiP","Offset_Spl","Offset_Lin")
###########################################################
# Display the first 10 results
###########################################################
round(thepredictions[1:10,],2)
###########################################################
# Display the results 41-50
###########################################################
round(thepredictions[41:50,],2)
###########################################################
# Save the predictions as a .csv data set
###########################################################
write.csv(round(thepredictions,2),file="Cuttoff_Predictions.csv")
