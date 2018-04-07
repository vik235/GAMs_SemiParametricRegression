###########################################################
# This is the Indiana growth curve data. It simply
# reproduces Figure 4.6 in the HRW book.
#
# Please note that this plot is very specialized, and 
# would not work if there were 100 subjects
#
# To me, it looks like the random intercept model works
# perfectly fine
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
#setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Indiana_Growth")
###########################################################
# Load the libraries
###########################################################
library(HRW)
library(lattice)
###########################################################
# Get the data for Black Males
###########################################################
data(growthIndiana)
growthINblackMales = growthIndiana[(growthIndiana$male == 1)
                                    &(growthIndiana$black == 1),] 
###########################################################
# What are the variables
###########################################################
names(growthINblackMales)
###########################################################
# How many unique individuals
###########################################################
aa = length(unique(growthINblackMales$idnum))
cat('Number of black males in the Indiana data set = ',aa,"\n")


###################################################
# Get the library you need for analyzing 
# the Indiana Growth Curve Data
###################################################
library(nlme)   
###################################################
# Get the crucial variables
###################################################
age = growthINblackMales$age
height = growthINblackMales$height
idnum =  growthINblackMales$idnum
###################################################
# Get ID numbers that are consecutive from 
# 1 to n. I have no idea why you need to do this
###################################################
idnumBM = rep(NA,length(idnum))
uqID = unique(idnum)
for (i in 1:length(uqID))
  idnumBM[idnum == uqID[i]] = i
growthINblackMales$idnum = idnumBM



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
##################################################################### 
# Set up the random effects structure and call lme
#
# See Lecture 18 for the explanation
##################################################################### 
#
dummyId = factor(rep(1,numObs))
Zblock   = list(dummyId = pdIdent( ~ -1 + Zgbl),
               idnumBM = pdSymm( ~ age),
               idnumBM = pdIdent( ~ -1 + Zgrp))

blkMalGD = groupedData(height ~ age|rep(1,length = numObs),
                        data = data.frame(height,age,Zgbl,Zgrp,idnumBM))
fit      = lme(height ~ age,data = blkMalGD,random = Zblock)
##################################################################### 
# What is in the fit
##################################################################### 
names(fit)
##################################################################### 
# Get the summary
summary(fit)
##################################################################### 
# Get sigma_{\epsilon}
##################################################################### 
cat('sigma_{epsilon} = ',fit$sigma,"\n")
##################################################################### 
# Get the loglikelihood
##################################################################### 
cat('Loglikelihood of the fitted model= ',fit$logLik,"\n")





##################################################################### 
# Now we are going to get BLUPs and plot functions, This is for the popn level estimates. 
# I will comment on most of the code
#####################################################################  
# Size of grid on which to do the plotting
ng = 101
# Get the grid of ages
ageg = seq(range.age[1],range.age[2],length = ng)
# Form the design matrix for the linear part, which is a column
# of ones plus the grid of ages
Xg = cbind(rep(1,ng),ageg)
# Spline terms for the overall fit
Zgblg = ZOSull(ageg,range.x = range.age,
                intKnots = intKnotsGbl)
# Spline terms for the individual fits
Zgrpg = ZOSull(ageg,range.x = range.age,
                intKnots = intKnotsGrp)
# Get betahat, the intercept and the slope
betaHat = as.vector(fit$coef$fixed)
# Get uHat, the estimated spline coefficients for the
# overall fit
uHat = as.vector(fit$coef$random[[1]])
# Form the overall fit
fHatg = as.vector(Xg%*%betaHat + Zgblg%*%uHat)
# Now get the subject-specific estimated curves
curvEsts = vector("list",numGrp)
for (i in 1:numGrp)
{
  # T^he subject-specific terms for the slope
  # and intercept
  uLinHati = as.vector(fit$coef$random[[2]][i,])
  # The subject-specific terms for the spline coefficients
  uSplHati = as.vector(fit$coef$random[[3]][i,])
  # The individual function estimates
  ghati = Xg%*%uLinHati + Zgrpg%*%uSplHati
  curvEsts[[i]] = fHatg + ghati
}
figBlkMalFit = xyplot(height ~ age|idnumBM,groups = idnumBM,
                       data = growthINblackMales,
                       strip = FALSE,scales = list(cex = 1.25),
                       xlab = list("age (years)",cex = 1.5),
                       ylab = list("height (centimeters)",cex = 1.5),
                       as.table = TRUE,layout = c(4,7),
                       panel = function(x,y,subscripts,groups)
                       {  
                         panel.grid()
                         adolNum = idnumBM[subscripts][1]
                         panel.superpose(x,y,subscripts,groups,
                                         col = "dodgerblue",type = "b")
                         panel.xyplot(ageg,curvEsts[[adolNum]],
                                      col = "blue",type = "l")
                       })
#pdf('BLUP_Fits_At_the_Individual_Level.pdf')
plot(figBlkMalFit)
#dev.off()

##################################################################### 
# Plot the overall function
##################################################################### 
#pdf('BLUP_Fit_At_the_Population_Level.pdf')
plot(ageg,fHatg,type='l',lwd=3,col="blue",
     xlab="Age",ylab="Height (cm)",
     main="Indiana Growth for Black Males")
#dev.off()
