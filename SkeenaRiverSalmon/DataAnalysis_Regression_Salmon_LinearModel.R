####
####Skeena River Sockeye Salmon Fishery data. We will be doing a simple linear regression of this dataset 
####The explanatory variable, X, in this dataset is number of spawners (in thousands) i.e number of mother fish
####The response variable is, Y, is the number of recruits (in thousands) i.e. number of teenage fish migrating into the ocean.
####
####

##We first set the working directory of the data set. It is this directory where the required files are place.
setwd("F:/OneDrive<>")

##Clean the workspace and set the seed so that results can be reporduced. 

rm(list = ls())
set.seed(4428967)

## Read the data into a frame called fishery
fishery = read.csv("Skeena_River(1).csv")

##Attach the dataset so that the column names can directly be used in modelling or plotting. This may be dangerous
attach(fishery)
summary(fishery)
str(fishery)

## Do some plotting to visualize the relation ship between Spawners and Recruits. Also we will plot densities of 
## of both X and Y to see the behaviour of covariate and he response. 

#### We see from the first plot that the relation is fairly linear and that the covariate and predictor are reasonably normal. 
#### There are humps in the density function. We also run normality text for both X and Y and found a good normal fit. 
#### Also note that we only have 28 data points so the power of these tests are pretty low.

par(mfrow=c(2,2))
plot(Spawners , Recruits , pch = "*" , cex = 2, main = "Skeena River Sockeye Salmon Fishery")
identify(Spawners , Recruits)
plot(density(Spawners) , main = "Sample density of Spawners")
plot(density(Recruits) , main = "Sample density of Recruits")
shapiro.test(Recruits) # pvalue 0.2007
shapiro.test(Spawners) # pvalue 0.5222
qqnorm(Recruits , main = "Normal refernece plot of Recruits")

###################Answers to Question 1################### 
#### We Perform a linear regressiomn between X and Y and check for any statisitcal significant relationship between X and Y
#### Note that this check is coonditional on validity of the model. We will check for this later

fit.1 = lm(Recruits ~ Spawners)
summary(fit.1)

###################Answers to Question 1 & 2 ################### 
#### We note that est coefficient for spwaners is 1.4414 with SE 0.4233 and significance of .00216 which is statistically significant 
#### This conforms to what we saw earlier as well. We thus conclude a statisitcal correlation between X and Y. 

#### We now check for heterescedasticity of the residuals and also add a non parametric and smoothed trend line to the plot. 
#### We notice that the assumption of constant variance is residuals for a given X is violeted in the plots as shown below

plot(Spawners , fit.1$residuals , main = "Residual Plot" , ylab = "Residuals" , pch= "*" , cex = 2)
lines(Spawners , predict(loess(((fit.1$residuals)) ~ Spawners)) , col = 2 , type = 'p')

plot(Spawners , abs(fit.1$residuals) , main = "Residual Plot" , ylab = "Abs Residuals" , pch= "*" , cex = 2)
lines(Spawners , predict(loess((abs(fit.1$residuals)) ~ Spawners)) , col = 2 , type = 'p')

plot(Spawners , sqrt(abs(fit.1$residuals)) , main = "Residual Plot" , ylab = "SqRt Abs Residuals" , pch= "*" , cex = 2)
lines(Spawners , predict(loess(sqrt(abs(fit.1$residuals)) ~ Spawners)) , col = 2 , type = 'p')

###################Answers to Question 3 ################### 
#### Regression is performed on log log transform of the variables and points 1951(12) and 1955(16) are marked 
#### which seems influential points. 
#### We also checked on how the density of the transformed variable changed after transforms. 
#### We see that we have lost the normality as was previously present. 

dev.off()
par(mfrow = c(2,2))
plot(log(Spawners) , log(Recruits) , pch = "*" , cex = 2, main = "Salmon Fishery after Log transforms")
identify(log(Spawners) , log(Recruits))
plot(density(log(Spawners) ) , main = "Sample density of log Spawners")
plot(density(log(Recruits)) , main = "Sample density of log Recruits")
shapiro.test(log(Recruits)) # pvalue 0.07267
shapiro.test(log(Spawners)) # pvalue 0.06575
qqnorm(log(Recruits) , main = "Normal refernece plot of log Recruits")

#### Regressing log(Y) to log(X)
fit.2 = lm(log(Recruits) ~ log(Spawners))
summary(fit.2)
fishery
###################Answers to Question 4 ################### 
#### We now check for heterescedasticity of the residuals and also add a non parametric and smoothed trend line to the plot. 
#### We notice that the assumption of constant variance is residuals for a given X is reasonably gone better and 
#### is better than the previous plots. The 2 points identified earlier are causing to non constant variance in the trend of residuals.

plot(log(Spawners) , fit.2$residuals , main = "Residual Plot" , ylab = "Residuals" , pch= "*" , cex = 2)
lines(log(Spawners) , predict(loess(((fit.2$residuals)) ~ log(Spawners))) , col = 2 , type = 'p')

plot(log(Spawners) , abs(fit.2$residuals) , main = "Residual Plot" , ylab = "Abs Residuals" , pch= "*" , cex = 2)
lines(log(Spawners) , predict(loess((abs(fit.2$residuals)) ~ log(Spawners))) , col = 2 , type = 'p')

plot(log(Spawners) , sqrt(abs(fit.2$residuals)) , main = "Residual Plot" , ylab = "SqRt Abs Residuals" , pch= "*" , cex = 2)
lines(log(Spawners) , predict(loess(sqrt(abs(fit.2$residuals)) ~ log(Spawners))) , col = 2 , type = 'p')


###################Answers to Question 5 ################### 
dev.off()
data = data.frame("Year" = Year , "Spawners" = log(Spawners) , "Recruits" = log(Recruits))
ord = order(log(Spawners))
x = log(Spawners)[ord]
y = log(Recruits)[ord]

fit.2 = lm(y ~ x)
pred = predict(fit.2 , newdata = as.data.frame(data) ,se.fit = TRUE)
n = nrow(fishery)
plot(x,pred$fit,type="l",col="blue",lwd=5,xlab="Log Spawners",ylab="Fitted values,  Log Recruits",
     main="Skeena River Sockeye Salmon Fishery")
t = qt(0.975,df=n-2)
upperCI = pred$fit+t*pred$se.fit
lowerCI = pred$fit-t*pred$se.fit

polygon(x=c(x, rev(x)), y=c(upperCI, rev(lowerCI)), col="gray", border=NA)
lines(x,pred$fit,col="blue",lwd=5)
plot(fit.1)
