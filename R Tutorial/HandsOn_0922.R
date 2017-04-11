##### HANDS ON #####

#    Amongst people with chronic diseases, those who *also* have mental health 
#    issues have considerably higher healthcare utilization.  This is a stong 
#    economic arguemnt for increased use of Cognitive Behavioral Therapy [CBT]
#    This MEPS data is broadly consistent with that message.

# These "hands on" modules will explore the relationship between mental health and 
# Medical Expenditures (i.e. healthcare utilization)
# We will explore a few specific questions:
#   1. Do people with more severe mental health issues have higher expenditures?
#   1b. How is mental health related to other predictive variables?
#   2. Does the relationship between mental health and expenditures still hold after accounting for predictive variables?
#   3. Does the impact of mental health on expenditures depend on RACE? 
#   3b. Is the impact of comorbidities on expenditures exacerbated by mental health issues?


##### Exploratory Data Analysis #####

library(plyr)
library(ggplot2)
# Read in data
setwd("C:\\Users\\avesper\\Desktop\\R Course Material\\Data\\")
meps <- read.csv("MEPS.Diabetes.SOA.csv")

table(meps$SMOKER)
# Remove observations where SMOKER = 99
meps2 <- subset(meps,SMOKER<99)

# Consider the MENTHEALTH variable
table(meps2$MENTHEALTH)
# It is a numeric variable
# Let's recode as a factor
meps2$MENTHEALTH <- as.factor(meps2$MENTHEALTH)
levels(meps2$MENTHEALTH)
# Change the levels to None, Low, Medium, High
meps2$MENTHEALTH <- mapvalues(meps2$MENTHEALTH, c("1","2","3","4"), c("None","Low","Medium","High"))
table(meps2$MENTHEALTH)
# Finally, use the plyr 'rename' command to change the variable name
meps2 <- rename(meps2,c('MENTHEALTH'='MHRANK')) 

# Create boxplots for each MHRANK
# Note that we could either create the LOGEXPEND variable
#   or use the log='y' command in boxplot
boxplot(EXPEND ~ MHRANK, data = meps2, log='y', main="MHRANK", varwidth=T, col='lavender')
abline(h=median(meps2$EXPEND), col="blue", lwd=3)

# Histogram of age
hist(meps2$AGE)
# Plot of log(Expend) vs. age
plot(log(EXPEND)~AGE,data=meps2)
# Add trend line
abline(coef(lm(log(EXPEND)~AGE,data=meps2)),col='red')

# Using ggplot
ggplot(data=meps2,aes(x=AGE,y=EXPEND)) + geom_point() + scale_y_continuous(trans = "log") + geom_smooth(method='lm')


##### Linear Regression #####

# Create the response variable
meps2$LOGEXPEND <- log(meps2$EXPEND)

# Compute the basic linear model
mod0 <- lm(LOGEXPEND~MHRANK,data=meps2)
summary(mod0)
# MHRANK is highly predictive by itself

# Recreate the race variables
meps2$BLACK <- I(meps2$RACE=='2 black')*1
meps2$WHITE <- I(meps2$RACE=='1 white')*1
meps2$OTHER <- 1-meps2$BLACK-meps2$WHITE
# Recreate LOGINCOME
meps2$LOGINCOME <- log(meps2$INCOME)

# Full model
mod1 <- lm(LOGEXPEND~MHRANK+BLACK+OTHER+LOGINCOME+AGE+BMI+FEMALE+HISPANIC+EMPLOYED+UNINSURED+MEDICAID+NOSEATBELT+SMOKER+CORONARY+CANCER+HIGHBP+EMPHYSEMA+ASTHMA+CHOLEST+STROKE,data=meps2)
summary(mod1)
# Why did the coefficients on MHRANK become smaller?

# Add interaction with race variables and MHRANK
mod2 <- lm(LOGEXPEND~MHRANK*(BLACK+OTHER)+LOGINCOME+AGE+BMI+FEMALE+HISPANIC+EMPLOYED+UNINSURED+MEDICAID+NOSEATBELT+SMOKER+CORONARY+CANCER+HIGHBP+EMPHYSEMA+ASTHMA+CHOLEST+STROKE,data=meps2)
summary(mod2)

# Add interaction with comorbidity variables
mod3 <- lm(LOGEXPEND~MHRANK*(BLACK+OTHER)+LOGINCOME+AGE+BMI+FEMALE+HISPANIC+EMPLOYED+UNINSURED+MEDICAID+NOSEATBELT+SMOKER+MHRANK*(CORONARY+CANCER+HIGHBP+EMPHYSEMA+ASTHMA+CHOLEST+STROKE),data=meps2)
summary(mod3)

anova(mod3,mod2,mod1)
# While MHRANK is significant, there's no evidence that its effect is mitigated by race or comorbidities

# Plot residuals and normal probability plot
plot(mod1$resid~mod1$fitted)
abline(h=0)
qqnorm(mod1$resid)
qqline(mod1$resid)

# Split data into train and holdout
train <- subset(meps2,PANEL==13)
holdout <- subset(meps2,PANEL==14)

# Re estimate model
mod1_train <- lm(formula(mod1),data=train)
mod3_train <- lm(formula(mod3),data=train)

# MAD holdout
mean(abs(predict(mod1_train,newdata=holdout)-holdout$LOGINCOME))
mean(abs(predict(mod3_train,newdata=holdout)-holdout$LOGINCOME))
# The simpler model performs better


##### GLM #####

# Edit parameters such as:
# dependent variable of choice
# link function
# transformations of independent variables

# Basic logistic regression with just a single variable
r0 <- glm(CANCER~SMOKER,family=binomial(link='logit'),data=meps2)
summary(r0)

# Add a few other explanatory variables
r1 <- glm(CANCER~SMOKER+AGE+BMI+UNINSURED+LOGINCOME,family=binomial(link='logit'),data=meps2)
summary(r1)

# Full model
r2 <- glm(CANCER ~ SMOKER+AGE+BMI+HISPANIC+EMPLOYED+UNINSURED+MEDICAID+NOSEATBELT
                  +EMPHYSEMA+STROKE+CORONARY+CHOLEST+ASTHMA+HIGHBP,family=binomial(link='logit'),data=meps2)
summary(r2)
summary(step(r2))

library(rpart)
# Build a simple tree
results <- rpart(CANCER~SMOKER+AGE+BMI+UNINSURED+LOGINCOME, method="anova", data=meps2)
# Plot results
plot(results)
text(results)

# Build a full tree
results2 <- rpart(CANCER ~ SMOKER+AGE+BMI+HISPANIC+EMPLOYED+UNINSURED+MEDICAID+NOSEATBELT
                  +EMPHYSEMA+STROKE+CORONARY+CHOLEST+ASTHMA+HIGHBP, method="anova", data=meps2, control=list(cp=.001))
summary(results2)
# Plot the CP 
plotcp(results2)
# Trim the tree
pfit <- prune(results2, cp=results2$cptable[which.min(results2$cptable[,"xerror"]),"CP"])
plot(pfit, uniform=TRUE, main="Classification Tree for Cancer")
text(pfit, use.n=TRUE, all=TRUE, cex=0.8)


##### Clustering and PCA #####

# Creates the meps clustering data as a small subset of the meps data
# note the scale command so that these variables have mean 0 and sd 1
meps_cluster <- scale(meps2[,c('AGE','BMI','SMOKER')])
# Select the number of clusters
k <- 4 # number of clusters - we have to decide this in advance, before running the clustering
set.seed(652) # Sets random seed so that repeated test runs will give the same result

# Kmeans
km <- kmeans(meps_cluster, k)
clust.km <- km$cluster
# Create a plot
plot(BMI~AGE,data=meps_cluster,col=clust.km,pch=SMOKER)
# Roughly split by healthy/unhealthy and smoker/non-smokers

# Now perform hierarchical clustring
d.mat <- dist(meps_cluster, method="euclidean") # calculate a distance matrix between rows
clust.hier <- hclust(d.mat, method="ward.D") # perform clustering
# visualize 4 vs. 5 clusters
plot(clust.hier, hang=-1) # for a smaller, more legible graph, set n<-20 several lines above here, and re-run all code in between
rect.hclust(clust.hier, k=5, border="pink") # can set k=2,3,4,5,6, etc.
rect.hclust(clust.hier, k=4, border="dodgerblue") # can set k=2,3,4,5,6, etc.

# perform principal components analysis
pc1 <- prcomp(meps_cluster)
round(pc1$rotation, 3) # eigenvectors

pcs <- predict(pc1)  # compute PCs
# create a scree plot to help see the amount of variation explained by the PCs
variances <- apply(pcs, 2, var) # calculate variances of each variable in pcs dataset
sum(variances); ncol(dat); ncol(pcs)
barplot(variances, col="lightblue", ylab="variance", las=1)
title("Principal Components Analysis Scree Plot", col.main="navy")
abline(h=1:7, col="darkcyan")
abline(h=0)

# Percent of variance for each PC
pc1$sdev^2/sum(pc1$sdev^2)


##### Casual Inference Hands On --- MEPS Data ####

# Create a data set with the target (CANCER), treatment (SMOKER)
# and relevant covariates
meps_c <- meps2[,c('CANCER','SMOKER','LOGINCOME','AGE','BMI','FEMALE','BLACK','OTHER','NOSEATBELT')]
dim(meps_c)
# Compute the average covariate value for non-smokers
meps_X0_mean <- apply(meps_c[meps_c$SMOKER==0,3:9],2,mean)
# Compute the average covariate value for non-smokers
meps_X1_mean <- apply(meps_c[meps_c$SMOKER==1,3:9],2,mean)
meps_mean_diff <- meps_X1_mean - meps_X0_mean
# Interestingly non-smokers tend to be older and have higher BMI

# Compute the p-value for each covariate
# t.test function returns a p.value value accessed using the $ symbol
meps_p_value <- c()
# Note the for loop, repeats the calculation for each element in the series
for (cov in 3:9) {
  meps_p_value <- c(meps_p_value,
                   t.test(meps_c[meps_c$SMOKER==1,cov],meps_c[meps$SMOKER==0,cov])$p.value)
}
# combine vectors into a matrix
meps_balance <- cbind(meps_X1_mean, meps_X0_mean, meps_mean_diff, meps_p_value)
round(meps_balance, 2)
# p-values are all significant
# terrible covariate balance

# Fit propensity score model
propensity <- glm(SMOKER~.,data=meps_c[,-which(names(meps_c)%in%c('CANCER'))],family=binomial(link=logit))
# Compute propensity scores
pscores <- predict(propensity,type='response')
# Create boxplot of propensity scores by Smoker
boxplot(pscores[meps_c$SMOKER==1],pscores[meps_c$SMOKER==0],
        names=c("Treatment Group","Control Group"),main="Estimated Propensity Scores")

# Define bins
bins <- as.numeric(cut(pscores,breaks=quantile(pscores,c(0,.2,.4,.6,.8,1)),include.lowest=T))

# Trim data based on pscores
bins <- bins[pscores>=min(pscores[meps_c$SMOKER==1])]
meps_c <- meps_c[pscores>=min(pscores[meps_c$SMOKER==1]),]

# Creat treatment weights
tab <- table(bins, meps_c$SMOKER)
tab
treatweights <- tab[,2]

# Create an empty matrix to store results for each bin and each covariate for both treated and covariates
meanT <- matrix(NA,nrow=max(bins),ncol=7)
rownames(meanT) <- 1:5
colnames(meanT) <- colnames(meps_c)[3:9]
meanC <- meanT
for (bin in 1:5) {
  for (cov in colnames(meanT)) {
    meanT[bin,cov] <- mean(meps_c[bins==bin & meps_c$SMOKER==1,cov])
    meanC[bin,cov] <- mean(meps_c[bins==bin & meps_c$SMOKER==0,cov])
  }
}

# Compute weight
weights <- treatweights/sum(treatweights)

# Weight average by subclass weight
subclass_balance <- cbind(round(t(meanT)%*%weights, 2),
                          round(t(meanC)%*%weights, 2))
colnames(subclass_balance) <- c("X1_mean", "X0_mean")
# Overall balance
round(subclass_balance, 2)
# Can see that the balance is very good within each subclass

# Compute average treatment effect in each subclass
diff <- rep(NA,5)
vars <- rep(NA,5)
for (b in 1:5) {
  temp <- meps_c[bins==b,]
  diff[b] <- mean(temp$CANCER[temp$SMOKER==1])-mean(temp$CANCER[temp$SMOKER==0])
  vars[b] <- var(temp$CANCER[temp$SMOKER==1])/length(temp$CANCER[temp$SMOKER==1]) 
    var(temp$CANCER[temp$SMOKER==0])/length(temp$CANCER[temp$SMOKER==0])
}
# Appropriate weighting by subclass
pointest <- diff%*%weights
varest <- vars%*%weights^2
lb <- pointest-1.96*sqrt(varest)
ub <- pointest+1.96*sqrt(varest)
# Final treatment ffect
pointest
c(lb, ub)
# Not significant, but remember this is a population of Diabetes patients
# Can't generalize to the wider population

# Compare to standard test
summary(glm(CANCER~SMOKER,data=meps_c))
# Effect is slightly negative!
