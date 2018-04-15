###########################################################
# This is the Indonesian respiratory data set
# It is just a logistic random intercept gamm,
# but this time we want to get the fitted probabilities
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
###########################################################
# Fit the random intercept model for logistic regression
###########################################################
fit = gamm(respirInfec ~ s(age) + vitAdefic + female + height
            + stunted + visit2 + visit3 + visit4
            + visit5 + visit6,
            random = list(idnum = ~1),
            family = binomial,data = indonRespir)
############################################################ Now get proper confidence intervals for females who are
# Vitamin A sufficient. In the data set, Vitamin A deficiency
# comes before gender
############################################################
# Number of grid points
ng = 101
# Grid on age
ageg = seq(min(indonRespir$age),max(indonRespir$age),
            length = ng)
# Average the covariates, with are 4-12 in the list
aveOthers = apply(indonRespir[,4:12],2,mean)
# Set the the females who are Vitamin A sufficient and not stunted
aveOthers[2] = 1
aveOthers[1] = 0
aveOthers[5] = 0
# ForM the new data frame
newDataDF = as.data.frame(cbind(ageg,
                           matrix(rep(aveOthers,each = ng),
                          ng,length(aveOthers))))
# The first two items in the data set are idnum and 
# the response. Get all the other names
names(newDataDF) = names(indonRespir)[-c(1,2)]
# Make it a data list
newDataList = as.list(newDataDF)
# Get the fitted logits
predObj = predict(fit$gam,newdata = newDataList,se = TRUE)
# Get the fitted probabilities
muHatg = 1/(1+exp(-predObj$fit))
aa     = predObj$fit + 2*predObj$se
bb     = predObj$fit - 2*predObj$se
lowergg = 1 / (1 + exp(-bb)) 
uppergg = 1 / (1 + exp(-aa)) 
ylimVal = range(c(0,max(uppergg)))

#pdf("Indonesian_Probabilities_Proper_Females_Sufficient_NotStunted.pdf")
plot(ageg,muHatg,type="n",,ylim=ylimVal,xlab="age in years",
     ylab="Respiratory Infection Probability",
     main="Indonesian Data, Proper CI",
     cex.lab = 1.5,cex.axis = 1.5,lwd=3)
polygon(c(ageg,rev(ageg)),c(lowergg,rev(uppergg)),
        col = "palegreen",border = FALSE)
lines(ageg,muHatg,col = "black",lwd=3)
rug(jitter(indonRespir$age),col = "dodgerblue")
#dev.off()

