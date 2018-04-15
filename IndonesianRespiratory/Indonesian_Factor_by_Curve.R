###########################################################
# This is the Indonesian respiratory data set
# It is just a logistic random intercept gamm,
# but this time we want to get the fitted probabilities
# of disease as a function of age with the fixed covariates
# set at their average values
#
# I will also try to fit the response probabilities
# when the children are female and Vitamin A deficient
# or suffient.
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
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Indonesian_Data")
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Indonesian_Data")
###########################################################
# Load the libraries
###########################################################
library(mgcv)
library(HRW)
###########################################################
# Get the data
###########################################################
data(indonRespir)
###########################################################
# Get the variable names and the unique values of Vitamin
# A deficiency. Deficiency = 0 and sufficiency = 1
###########################################################
names(indonRespir)
unique(indonRespir$vitAdefic)
vitAdefic = indonRespir$vitAdefic
respirInfec = indonRespir$respirInfec
cat('Fraction with infrections among those Vitamin A sufficient = ',
        mean(respirInfec[vitAdefic==0]),"\n")
cat('Fraction with infrections among those Vitamin A deficient  = ',
    mean(respirInfec[vitAdefic==1]),"\n")
female = indonRespir$female
cat('Fraction with infrections among females = ',
    mean(respirInfec[female==1]),"\n")
cat('Fraction with infrections among   males  = ',
    mean(respirInfec[female==0]),"\n")

###########################################################
# Fit the random intercept model for logistic regression
###########################################################
fit <- gamm(respirInfec ~ s(age) + vitAdefic + female + height
            + stunted + visit2 + visit3 + visit4
            + visit5 + visit6,
            random = list(idnum = ~1),
            family = binomial,data = indonRespir)
###########################################################
# Summarize
###########################################################
summary(fit$gam)
intervals(fit$lme,which="fixed")
###########################################################
# Plot the logits
###########################################################
#pdf("Indonesian Logits.pdf")
plot(fit$gam,shade = TRUE,shade.col = "palegreen",xlab="Age",ylab="Logits",
     main="Indonesian Data",lwd=3,col="black")
#dev.off()
###########################################################
# Now get confidence intervals
# These are not proper ones because they involve the delta
# method
###########################################################
# Number of grid points
ng <- 101
# Grid on age
ageg <- seq(min(indonRespir$age),max(indonRespir$age),
            length = ng)
# Average the covariates, with are 4-12 in the list
aveOthers <- apply(indonRespir[,4:12],2,mean)
# For the new data frame
newDataDF <- as.data.frame(cbind(ageg,
                                 matrix(rep(aveOthers,each = ng),
                                        ng,length(aveOthers))))
# The first two items in the data set are idnum and 
# the response. Get all the other names
names(newDataDF) <- names(indonRespir)[-c(1,2)]
# Make it a data list
newDataList <- as.list(newDataDF)
# Get the fitted logits
predObj <- predict(fit$gam,newdata = newDataList,se = TRUE)
# Get the fitted probabilities
muHatg <- 1/(1+exp(-predObj$fit))
# Get the delta-method standard errors of the probabilities
stdErrg <- predObj$se*muHatg*(1-muHatg)
# Form upper and lower limits
lowerg <- muHatg - 2*stdErrg 
upperg <- muHatg + 2*stdErrg
# Check to see if the delta method fails
cat('The minimum delta-method CI = ',min(lowerg),"\n")
cat('The maximum delta-method CI = ',min(upperg),"\n")
# Because of the use of the delta method, the intervals
# might not be in [0,1]. Artificially truncate them so that
# they are
ylimVal <- range(c(lowerg,upperg))
# Plot everything
#pdf("Indonesian_Response_Probabilities_DeltaMethod.pdf")
plot(ageg,muHatg,type="n",,ylim=ylimVal,xlab="age in years",
     ylab="Respiratory Infection Probability at mean of Covariates",
     main="Indonesian Data",
     cex.lab = 1.5,cex.axis = 1.5,lwd=3)
polygon(c(ageg,rev(ageg)),c(lowerg,rev(upperg)),
        col = "palegreen",border = FALSE)
lines(ageg,muHatg,col = "black",lwd=3)
rug(jitter(indonRespir$age),col = "dodgerblue")
# dev.off()

#############################################################
# Now get that CI plot but without using the delta method
#############################################################
aa     = predObj$fit + 2*predObj$se
bb     = predObj$fit - 2*predObj$se
lowergg <- 1 / (1 + exp(-bb)) 
uppergg <- 1 / (1 + exp(-aa)) 
# pdf("Indonesian_Response_Probabilities_ProperMethod.pdf")
plot(ageg,muHatg,type="n",,ylim=ylimVal,xlab="age in years",
     ylab="Respiratory Infection Probability",
     main="Indonesian Data, Proper CI",
     cex.lab = 1.5,cex.axis = 1.5,lwd=3)
polygon(c(ageg,rev(ageg)),c(lowergg,rev(uppergg)),
        col = "palegreen",border = FALSE)
lines(ageg,muHatg,col = "black",lwd=3)
rug(jitter(indonRespir$age),col = "dodgerblue")
#dev.off()

