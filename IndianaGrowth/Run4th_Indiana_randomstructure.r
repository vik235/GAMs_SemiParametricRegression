##################################################################### 
# Set up the random effects structure and all lme
#
# See Lecture 18 for the explanation
##################################################################### 
#
dummyId = factor(rep(1,numObs))
Zblock   = list(dummyId = pdIdent( ~ -1 + Zgbl), 
               idnumBM = pdSymm( ~ age), # The between people slope and intercepts U1i and U0i
               idnumBM = pdIdent( ~ -1 + Zgrp))

blkMalGD = groupedData(height ~ age|rep(1,length = numObs),
                        data = data.frame(height,age,Zgbl,Zgrp,idnumBM))
fit      = lme(height ~ age,data = blkMalGD,random = Zblock)
##################################################################### 
# What is in the fit
##################################################################### 
names(fit)
##################################################################### 
# Get the summary, #Look lecture 19 notes
summary(fit)
##################################################################### 
# Get sigma_{\epsilon}
##################################################################### 
cat('sigma_{epsilon} = ',fit$sigma,"\n")
##################################################################### 
# Get the loglikelihood
##################################################################### 
cat('Loglikelihood of the fitted model= ',fit$logLik,"\n")
