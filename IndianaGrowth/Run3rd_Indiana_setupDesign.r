###Manually setting up splines basis functions for Age , 
###i.e. for all the fixed effects , Z_gbl and for the random comp of individuals Z_grp
#######################################################
# Next, set up the design matrices for the splines
# at the population level and at the individual level.
#
# The individual level should have less knots than
# the population level
#
# The range.age statement uses the fact that the 
# minimum age is 5.7 and the maximum is 19.98
# The range should include this. In earlier code the
# range was set without need to know the data
#######################################################
# Total number of observations
numObs = length(age)
# Number of subjects
numGrp = length(unique(idnum))
# Number of knots at the population level
numIntKnotsGbl = 20
# O'Sullivan Basis Functions at the population level
intKnotsGbl = quantile(unique(age),
      seq(0,1,length=numIntKnotsGbl+2))[-c(1,numIntKnotsGbl+2)]

summary(growthINblackMales$age)
range.age = c(5.5,20)
range.age = c(min(age)-0.01, max(age)+0.01)
Zgbl = ZOSull(age,range.x=range.age,intKnots=intKnotsGbl)
# Number of knots at the subject level. Should be less than the
# number of knots at the population level
numIntKnotsGrp = 10	
# Basis functions for O'Sullivan splines at the 
# individual level
intKnotsGrp = quantile(unique(age),
                        seq(0,1,length=numIntKnotsGrp+2))[-c(1,numIntKnotsGrp+2)]
Zgrp = ZOSull(age,range.x=range.age,intKnots=intKnotsGrp)


