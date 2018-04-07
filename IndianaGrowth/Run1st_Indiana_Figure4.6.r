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
############################################################
# Load the libraries
###########################################################
library(HRW)
library(lattice)
###########################################################
# Get the data for Black Males
###########################################################
data(growthIndiana)
growthINblackMales <- growthIndiana[(growthIndiana$male == 1)
                                    &(growthIndiana$black == 1),] 
###########################################################
# What are the variables
###########################################################
names(growthINblackMales)
head(growthINblackMales , 10)
###########################################################
# How many unique individuals
###########################################################
aa = length(unique(growthINblackMales$idnum))
cat('Number of black males in the Indiana data set = ',aa,"\n")
###########################################################
# Plot the trajectory for all the subject. This uses
# the R function xyplot within the R package lattice
#
# The HRW book refers to other sources to get these plots.
# Since there are 28 subjects, it decides on 7 rows and 4 
# columns
###########################################################
figBlkMalRaw <- xyplot(height ~ age|idnum,groups=idnum,
                       data = growthINblackMales,
                       layout = c(4,7),
                       strip = FALSE,scales = list(cex = 1.25),
                       xlab = list("age (years)",cex = 1.5),
                       ylab = list("height (centimeters)",
                                   cex = 1.5),as.table = TRUE,
                       panel = function(x,y,subscripts,groups)
                       {  
                         panel.grid()
                         panel.superpose(x,y,subscripts,groups,
                                         col = "blue",type = "b")
                       })

#pdf("Indiana_LatticePlot.pdf")
plot(figBlkMalRaw) 
#dev.off()