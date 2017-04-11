library(MASS)
library(psych)
library(ggplot2)
# Reading data from csv file with cases corresponding to lines and variables to fields in the file

setwd("C:\\Users\\avesper\\Desktop\\R Course Material\\Data\\")
meps <- read.csv("MEPS.Diabetes.SOA.csv")

# Plotting histogram of variable of interest, in this case the "EXPEND" variable from "meps" data frame
truehist(meps$EXPEND, xlab="Expend", col='lavender')
# Add a kernel density
lines(density(meps$EXPEND),col='navy',lwd=2)

# Change the title of the current plot, as well as the color
title("EXPEND Distribution", col.main="navy")

# Next we want dig into the distribution of expend. For example, a simple question we want to figure out is
# how expensive is top 20% patients 

# Extract the column of expend from table
EXPEND <- meps$EXPEND

# Arrange the data in decreasing order
# order is a function that returns a permutation which rearranges its argument
EXPEND <- EXPEND[order(EXPEND, decreasing=TRUE)]

# Create a variable of the cumulative percentage of expend 
cum.exp <- cumsum(EXPEND)/sum(EXPEND)

# Create a variable of the cumulative percentage of population
n <- length(EXPEND)
cum.n <- (1:n)/n

# Plot the cumulative expenditures

plot(cum.n, cum.exp, type="l", lwd=3, col="blue")

# Plot grid lines
qq <- seq(0, 1, .05)
abline(h=qq, col="lightgrey")
abline(v=qq, col="lightgrey")

# Plot diagnoal line
abline(0,1,col="navy",lwd=3)

# Identify how expensive is top 20% patients
exp.20 <- cum.exp[which(cum.n == 0.2)]

# Two straignt dotted lines
segments(.2,-1,.2,exp.20, col='red', lwd=4, lty=3)
segments(-1,.67,.2,exp.20, col='red', lwd=4, lty=3)

# Plot the point on the curve
points(.2,exp.20,col="red",pch=16, cex=2)

# Change the titile of the plot
title("Analysis of Cumulative Expenditures", col.main="navy")

# 20% of patients account for about 70% of Expenditures

# look at log(EXPEND)
truehist(log(meps$EXPEND), xlab="", col='lightgreen', main="LOGEXPEND")

# Create log expend and log income variables
meps$LOGEXPEND <- log(meps$EXPEND)
meps$LOGINCOME <- log(meps$INCOME)

# Continuous variable visualization
#	use a scatterplot matrix (SPLOM)
# SPLOM plots histogram on the diagnoal, bivariate scatter plots below the diagonal
# and the Pearson correlation above the diagonal
# use pairs.panels function from psych package
temp <- meps[,c("INCOME", "LOGINCOME", "BMI", "AGE", "LOGEXPEND")]
pairs.panels(temp, pch='.', gap=0, ellipses=F, hist.col='lavender', col="blue", lwd=3)

# Discrete variable visualization
#	use boxplots
# Box plots display the distribution of data based on 
# the five number summary: minimum, first quartile, median, third quartile, and maximum
# Outliers are either 3×IQR or more above the third quartile or 3×IQR or more below the first quartile.
# Suspected outliers are are slightly more central versions of outliers: either 1.5×IQR 
# or more above the third quartile or 1.5×IQR or more below the first quartile.
# Box plotting by groups is a good way to spot difference in distribution of a continuous variable
# by a discrete variable

# Determine how many stroke patients are there
table(meps$STROKE)
# Create a boxplot of the data
boxplot(EXPEND ~ STROKE, data = meps, log='y', main="STROKE", varwidth=T, col='lavender')
abline(h=median(EXPEND), col="blue", lwd=3)
# Is this difference statistically significant?
# Conduct a two sample t-test
# Use LOGEXPEND to satisfy Normality assumptions of t.test (more discussion in next module)
t.test(LOGEXPEND~STROKE,data=meps)

boxplot(EXPEND ~ CANCER, data = meps, log='y', main="CANCER", varwidth=T, col='lavender')
abline(h=median(EXPEND), col="blue", lwd=3)

boxplot(EXPEND ~ HISPANIC, data = meps, log='y', main="HISPANIC", varwidth=T, col='lavender')
abline(h=median(EXPEND), col="blue", lwd=3)

boxplot(EXPEND ~ RACE.RAW, data = meps, log='y', main="RACE", varwidth=T, col='lavender')
abline(h=median(EXPEND), col="blue", lwd=3)

# ggplot2 example
# ggplot2 make creating plots of multivariate data easier
# It is also easier to create customized and novel plots with ggplot2
# In the next example, we use ggplot to generate box plots to look at expend by race and gender
# GGplot means implementation of the Grammar of Graphics, hence gg-plot
# The basic notion is that there is a grammar to the composition of graphical components 
# in statistical graphics, and by direcly controlling that grammar, 
# you can generate a large set of carefully constructed graphics tailored to your particular needs. 
# Each component is added to the plot as a layer

ggplot(meps, aes(RACE.RAW, LOGEXPEND)) +
  geom_boxplot()

# All ggplot2 plots begin with the function ggplot(). ggplot() takes two primary arguments:
#  data
# The data frame containing the data to be plotted
#  aes()
# The aesthetic mappings to pass on to the plot elements
# As you can see, the second argument, aes(), isn't a normal argument, 
# but another function. Since we'll never use aes() as a separate function, 
# it might be best to think of it as a special way to pass a list of arguments to the plot.
# You add geometries to a plot with one of the geom_*() functions, 
# using the + operator. To see a full list of available geometries, look at the ggplot2 webpage under "Geoms"

# Some of these geometries have their own particular aesthetics. For instance:
#   points
#     point shape
#     point size
#   lines
#     line type
#     line weight
#   bars
#     y minimum
#     y maximum
#     fill color
#     outline color
#   text
#     label value

# Here we use fill to plot color of bar by gender
ggplot(meps, aes(RACE.RAW, LOGEXPEND, fill = factor(FEMALE)))+
  geom_boxplot()


# Consider an Income by Hispanic interaction
# Plot LOGEXPEND ~ LOGINCOME
plot(LOGEXPEND~LOGINCOME,data=meps)
abline(lm(LOGEXPEND~LOGINCOME,data=meps))
# Separately for men and women
# Plot HISPANIC in red
# note the data subset selecting rows where HISPANIC = 1
plot(LOGEXPEND~LOGINCOME,data=meps[meps$HISPANIC==1,],col='red')
# Add a trend line for the regression
abline(lm(LOGEXPEND~LOGINCOME,data=meps[meps$HISPANIC==1,]),col='red',lwd=2)
# Plot non-HISPANIC in blue
points(LOGEXPEND~LOGINCOME,data=meps[meps$HISPANIC==0,],col='blue')
abline(lm(LOGEXPEND~LOGINCOME,data=meps[meps$HISPANIC==0,]),col='blue',lwd=2)

# Using ggplot
# geom_point creates a scatterplot
# geom_smooth adds a trend line
ggplot(meps,aes(LOGINCOME,LOGEXPEND,color=factor(HISPANIC)))+geom_point()+geom_smooth(method=lm,se=F)