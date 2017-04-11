###################### Introduction to Data Science in R #######################

##########################################################
#	unsupervised learning case study
#	analysis of county-level health data
#
#	University of Wisconsin Population Health Institute
#	County Health Rankings 2013
#	Accessible at www.countyhealthrankings.org
##########################################################

# load libraries
rm(list = ls())
library(MASS)
library(psych)
library(corrplot)
library(FactoMineR)
library(ggplot2)
library(ggfortify)
library(ggdendro)
####################################
#	prepare data
####################################

# Edit this next line to change the working directory to the file where you have
# saved the CountyHealthRR.csv data file
setwd("~/CED/R course")

dat <- read.csv("countyHealthRR.csv") # read in data from .csv file

dim(dat) # get dimensions (# rows, # columns)
head(dat) # view first few rows of data set
describe(dat) # view summary stats on some variables

# Now we want to remove some variables with a lot of missing data: 
var.descriptions <- describe(dat) # save the data frame of summary stats as "var.descriptions"
# save index positions of variable that have at least 2900 observations
keep <- var.descriptions$var[var.descriptions$n > 2900]
keep # print the index numbers of variables to keep
names(dat)[keep] # variables to keep
names(dat)[-keep] # variables to remove

dat <- dat[,keep] # keep only select varaibles
dim(dat) # how many variables are left?
dat <- na.omit(dat) # remove rows with missing data
dim(dat) # how many rows are left?

# save identifying variables (county name, etc.) separately
county.ID.vars <- dat[,1:3] # type names(dat)[1:3] to see which variables these are
summary(county.ID.vars) # view a summary of these variables

# save data set without ID variables for analysis
model.dat <- dat[,4:ncol(dat)] # starting with column 4, to ignore the first three identifying columns

##########################
# create correlation matrix
corr.mat <- cor(model.dat, use="pairwise.complete.obs")
# We compute a (symmetric) matrix of correlation coefficients 
# between the county-level measurements.
# The option "Pairwise-complete observations" means to ignore missing values when calculating correlations

# visualize correlation between variables
corrplot(corr.mat, order="hclust", method="shade", tl.cex=.7) 
# Rather than display a large matrix of numbers, we visualize the 
# correlation matrix using  a heatmap.



################################################################################
#  Clustering
#	Authors:	Kevin Coltin, Cristina DeFilippis, Deloitte Consulting LLP
#	Date:		  August, 2016
#	Purpose: 	Illustrate clustering and principal component analysis in the
#			      context of analyzing US county-level health/mortality patterns
# Adapted from code written by James Guszcza, Deloitte Consulting LLP
################################################################################

model.dat <- scale(model.dat) # scale data to mean 0 and standard deviation 1
# save data as data frame again- the output of "scale" is a matrix, not a data frame
model.dat <- as.data.frame(model.dat)

####################################
#	exploratory analysis
####################################
describe(model.dat) # note: variables have been centered and scaled (have mean 0, std deviation 1)

# create correlation matrix
corr.mat <- cor(model.dat, use="pairwise.complete.obs")

# visualize correlation between variables
corrplot(corr.mat, order="hclust", method="shade", tl.cex=.7)

###############################################################################################
#	perform clustering, using two algorithms - k-means and hierarchical clustering
###############################################################################################

### k-means clustering
k <- 5 # number of clusters - we have to decide this in advance, before running the clustering
set.seed(652) # Sets random seed so that repeated test runs will give the same result
n <- nrow(model.dat) # note: set n=20 to view small dendrogram example
km <- kmeans(model.dat[1:n,], k)
clust.km <- km$cluster

### hierarchical clustering 
d.mat <- dist(model.dat[1:n,], method="euclidean") # calculate a distance matrix between rows
clust.hier <- hclust(d.mat, method="ward.D") # perform clustering

# visualize 4 vs. 5 clusters
plot(clust.hier, hang=-1, labels=dat$County[1:n]) # for a smaller, more legible graph, set n<-20 several lines above here, and re-run all code in between
rect.hclust(clust.hier, k=5, border="pink") # can set k=2,3,4,5,6, etc.
rect.hclust(clust.hier, k=4, border="dodgerblue") # can set k=2,3,4,5,6, etc.

clust.labels <- cutree(clust.hier,4) # label data as belonging to a cluster 1-4
head(clust.labels) # see the first few cluster labels - note that clust.labels is just a vector of numbers

# visualize clusters
plot.new() # create a new, empty plot
# set the plot to the correct x- and y-dimensions by "plotting" all the points, but invisibly (using color 'white')
plot(model.dat$Perc.Low.Birth.Weight, model.dat$Perc.Obese, col='white')
title("Visualizing Clusters on 2 dimensions", mar=c(1,1,1,1))
colors <- c("purple", "blue", "green", "red")
# Now, loop through each of the clusters, and plot the points in each cluster in a different color
for (clust in 1:max(clust.labels)) {
  points(model.dat$Perc.Low.Birth.Weight[clust.labels == clust], 
         model.dat$Perc.Obese[clust.labels == clust], col=colors[clust], 
         pch=16) 
}
# Add a legend identifying the clusters
legend('bottomright', levels(as.factor(clust.labels)), 
       col=colors, pch=16, bty='n', cex=.75, title="Cluster")


# relabel clusters in terms of average mortality
# We're going to create a function here that will re-label the clusters so that Cluster #1 has the lowest
# average mortality, Cluster #2 has the second lowest, etc.
# For now, don't worry about the details of how this function works - just understand what it does and why
# we might want to use it.
reord <- function(cluster){
  avg <- tapply(scored$YPLL.Rate, cluster, mean)
  ord <- order(avg)
  clus <- factor(cluster, levels=ord)
  levels(clus) <- 1:length(clus)
  return( as.numeric(as.character(clus)) )
}

# include cluster labels in data set
scored <- model.dat # copy data set
scored$clust.km <- reord(clust.km) # add column for k-means cluster
scored$clust.hier <- reord(clust.labels) # add column for hierarchical cluster

####################################
# 	analyze clusters
####################################

## Print variable means by cluster 
agg <- function(x) tapply(x, scored[, "clust.km"], mean) # create a function to take the mean within each cluster
summ <- apply(model.dat, 2, agg) # apply that function to calculate the means of each variable, within each cluster
t(round(summ,2)) # t() means transpose - rotate the dataset to swap rows/columns. Round to 2 digits.

## Plot boxplots of select variables by cluster
names(dat) # print variable names
# select indexes of variables to make plots for 
show <- c(1:7,9)
names(dat)[show]

# select cluster colors
myCols <- c("blue", "dodgerblue", "lightgreen", "pink", "red", "maroon", "darkorange")

dev.new() # new plot
par(mfrow=c(2,4)) # set graph to show 8 plots at once
for(i in show){
  boxplot( scored[,i] ~ scored[,"clust.km"], col=myCols, varwidth=TRUE)
  abline(h=0, col="navy")
  title(names(scored)[i])
}

# repeat to make boxplots for more variables
show <- 10:17
par(mfrow=c(2,4)) # set graph to show 8 plots at once
for(i in show){
  boxplot( scored[,i] ~ scored[,"clust.km"], col=myCols, varwidth=TRUE)
  abline(h=0, col="navy")
  title(names(scored)[i])
}
par(mfrow=c(1,1))


################################################################################
#  PCA
#	Authors:	Kevin Coltin, Cristina DeFilippis, Deloitte Consulting LLP
#	Date:		  May, 2016
#	Purpose: 	Illustrate clustering and principal component analysis in the
#			      context of analyzing US county-level health/mortality patterns
# Adapted from code written by James Guszcza, Deloitte Consulting LLP
################################################################################

####################################
#	perform PCA
####################################
# perform principal components analysis
pc1 <- prcomp(model.dat)
pc1 <- prcomp(scale(model.dat))

round(pc1$rotation[,1:2], 3) # eigenvectors

pcs <- predict(pc1)	# compute PCs
describe(pcs)[,1:5] # mean zero
dim(pcs); dim(dat) # pcs and original data have same dimensions
corrplot(cor(pcs))	# the PCs are uncorrelated

# create a scree plot to help see the amount of variation explained by the PCs
variances <- apply(pcs, 2, var) # calculate variances of each variable in pcs dataset
sum(variances); ncol(dat); ncol(pcs)
dev.new() # create new empty plot
barplot(variances[1:10], col="lightblue", ylab="variance", las=1)
title("Principal Components Analysis Scree Plot", col.main="navy")
abline(h=1:7, col="darkcyan")
abline(h=0)

dev.new() # create new empty plot
plot(pc1)	# note that the same scree plot we created above results from the simple plot command!
summary(pc1)
# Determine what cumulative % of variance you want to account for to help 
# decide the number of PCs to include.
# In this case, our heuristic total within sum of squares analysis suggests that
# k=4 or k=5 are reasonable choices.


# visualize the data by plotting one PC vs. another
dev.new() # create new empty plot
biplot(pc1, col=c("grey", "blue"), cex=c(.2, .8))
round(pc1$rotation, 4)[,1:2]

# quick biplot using ggpplot2
autoplot(prcomp(model.dat), loadings = TRUE,  loadings.colour = 'blue', 
         loadings.label = TRUE, loadings.label.size = 3)

# We could also plot the data in the space of the 1st two PCs
# with the data points color-coded by the cluster assignment.
# Instead of the standard prcomp function we used before, here we'll use the PCA function, from
# the FactoMineR package
pc2 <- PCA(model.dat)
dev.new() # create new empty plot
plot(pc2, choix=c("ind"), label="none", col.ind=scored$clust.km)
# Notes: We see clear separation by color - suggesting that differences among 
# the cluster reside in the first two dimensions of this biplot. 
# Remember that the cluster analysis does not use the principal component scores. 
# There is no data reduction prior to the clustering.

# quick plot of data colored by cluster using ggpplot2
autoplot(kmeans(model.dat, 5), data = model.dat)
# plotting data points for two principal components shows clear separation of clusters
# so by looking at the make-up of the principal components we can tell what 
# separates the clusters


##################################################
##### HANDS ON #####

#
# JimG comment:  this is not terribly exciting because there is low
#   correlation amongst these three variables
#

meps <- read.csv("MEPS.Diabetes.SOA.csv")
meps$LOGINCOME <- log(meps$INCOME)

meps2 <- meps[,c('LOGINCOME','AGE','BMI')]
meps2 <- scale(meps2)

dd <- dist(meps2, method="euclidean")
hc1 <- hclust(dd, method="ward.D")
plot(hc1, hang=-1)
rect.hclust(hc1, k=5, border="dodgerblue") 

pc <- prcomp(meps2)
biplot(pc, col=c("slategrey", "navy"), cex=c(.2, .8))
round(pc$rotation, 4)[,1:2]


#############################################################
# code below added by Jim G
#
# alternate idea for hands-on PCA
# (I don't think the MEPS data is good to illustrate PCA)	
#
# using ACT and SAT score data 
# We could use it to illustrate PC regression
# as a way of handling multicollinearity
#############################################################

data(sat.act)
#?sat.act
dat <- sat.act
head(dat)
gender <- gender-1     # gender=1 is female
dim(dat)
summary(dat)
dat <- na.omit(dat)
dim(dat)
pairs.panels(dat[,4:6])


# run a PCA and add the first PC to the scatterplot matrix
pca <- prcomp(scale(dat[,4:6]))
plot(pca)		# scree plot shows 1st PC is a good summary
pca$rotation	# loadings 
summary(pca)
pcs <- predict(pca)
dim(dat); dim(pcs)
apply(pcs, 2, var)
sum(apply(pcs, 2, var))

pc1 <- pcs[,1]
dat <- data.frame(dat, pc1)
pairs.panels(dat)


# ask participants to run the sequence of models {r1,r2,r3}
# to predict higher education using age/gender/pc1
# demonstrates how PCA addresses the problem of multicollinerity

dat$ed45 <- I(dat$education>3)*1
r1 <- glm(ed45 ~ gender + age + ACT + SATV + SATQ
	, family="binomial", data=dat)
summary(r1)
r2 <- update(r1, .~. - SATV - SATQ)
summary(r2)
r3 <- update(r2, .~. - ACT + pc1)
summary(r3)
AIC(r1,r2,r3)
BIC(r1,r2,r3)

# AIC and BIC both like the principal component regression model best
