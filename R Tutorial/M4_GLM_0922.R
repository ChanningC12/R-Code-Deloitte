######################################################################
# Generalized Linear Models, Classification Trees, Random Forest, Regression Trees

# For use in "R for Data Science" training materials 

# Outline:

# LOAD DATA

# CASE STUDY 1 - Generalized Linear Model (Basics)
# METHOD: Simple regressions with different link functions
#         and variable transformations to predict expenditures

# CASE STUDY 2: Predict individuals with expenditures exceeding $10,000
# METHOD: Use random forest for variable selection, and then use 
#         resulting selected variables to compare performance between 
#         GLM and classification tree models

# CASE STUDY 3: Predict expenditure value for an individual
# METHOD: Use decision trees and then prune the tree to improve performance

# HAND-ON EXERCISE
######################################################################



# LOAD DATA

# removes all variables in the global environment to ensure clean starting point for analysis
rm(list = ls())

# finds the folder which has the data of interest
setwd("C:\\Users\\cmcgee\\Documents\\R Course\\")  

# Loads the MEPS diabetes data (from the folder specified above) into the variable 'meps'
meps <- read.csv("MEPS.Diabetes.SOA.csv")

# Takes the log of income and expenditure and defines these as two new variables 'LOGINCOME', 'LOGEXPEND'
# These two log variables will be used later in the analysis
meps$LOGINCOME <- log(meps$INCOME)
meps$LOGEXPEND <- log(meps$EXPEND)




################################################################################################

# CASE STUDY 1 - Generalized Linear Model (Basics)
# METHOD: Simple regressions with different link functions
#         and variable transformations to predict expenditures
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html

# Example 1.1: Univariate model (Gaussian)

# The function 'glm' has at least 4 arguments that define the regression model:
# 1) Dependent column variable (e.g. LOGEXPEND), followed by a '~', which separates dependent from independent variables
# 2) Independent column variable names, separated by '+' signs (but in the case below, there is only one)
# 3) data= for dataset name (e.g. data=meps since meps was generated above at dataload)
# 4) family= for error distribution and link function (e.g. gaussian)
# - The results of the glm function are stored in a variable, in this case defined as 'results_lm'
results_lm=glm(LOGEXPEND ~ LOGINCOME, data=meps, family=gaussian)

# print summary of results and confidence intervals
summary(results_lm)       # AIC: 48072, measure of model performance where lower is better
confint(results_lm)       # confidence interval

# create scatter plot of predicted vs. actual, with 2 arguments:
# 1) Dependent column variable (meps$LOGEXPEND, where meps is the variable and $LOGEXPEND is the column)
plot(meps$LOGEXPEND,predict(results_lm, type="response")) 
# plot histogram of residuals
hist(resid(results_lm))   # residuals not normally distributed

# Example 1.2: Univariate model (Gamma)
# Log link, coefficients have multiplicative impact on mean of EXPEND 
results_glm=glm(EXPEND ~ LOGINCOME, data=meps, family=Gamma(link="log"))
summary(results_glm)       # AIC: 43865 (lower AIC)
# Very similar to LM model
# Different assumptions on distribution of "error" term
confint(results_glm)       # confidence interval
plot(meps$EXPEND,predict(results_glm, type="response")) # predicted vs. actual
hist(resid(results_glm))   # residuals close to normally distributed
# Check for heteroskedasticity, i.e. the pattern in the residuals, X vs. Y
# The first argument is the independent variable (log of income), or X
# The second argument is the residual (actual minus predicted), or Y
plot(log(meps$INCOME),meps$EXPEND-predict(results_glm, type="response"))

# Example 1.3: Multivariate model (Gamma)
# This examples shows multiple independent column variables, separated by '+' signs
results=glm(EXPEND ~ log(INCOME) + AGE + BMI + FEMALE, data=meps, family=Gamma(link="log"))
summary(results)       # AIC: 43804, p-values significant except for FEMALE
confint(results)       # confidence interval
plot(meps$EXPEND,predict(results, type="response")) # predicted vs. actual
hist(resid(results))   # residuals close to normally distributed

# Check for multicollinearity in Example 1.3

# install the car package
install.packages('car') 
library(car)
vif(results) # Variance inflation factor printed for each variable



################################################################################################

# CASE STUDY 2: Predict individuals with expenditures exceeding $10,000
# METHOD: Use random forest for variable selection, and then use 
#         selected variables to compare performance between 
#         GLM and classification tree models

# if the expenditure column in the meps variable is greater than 10000, then the 
# new column 'greater10K' receives a value of 1. If the expenditure is less than 10000, then the
# new column 'greater10K' receives a value of 0
meps$greater10K <- ifelse(meps$EXPEND>10000, 1, 0)

# install packages for random forest and classification trees
install.packages('randomForest')
install.packages('tree')

# Example 2.1: Random forest for column variable selection
# https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
library(randomForest)
set.seed(415) # pick a random seed
# The first argument 'as.factor(meps$greater10K)' is the dependent column variable
# The second argument is a list of all column variables to test in the random forest, separated by '+'
# Third argument is the variable data=, in this case data=meps
# Fourth argument is importance=TRUE to show which variables contribute most to income classification
# ntree is number of trees to test
fit <- randomForest(as.factor(meps$greater10K) ~ INCOME+AGE+BMI+FEMALE+RACE.RAW+
                      HISPANIC+EMPLOYED+UNINSURED+MEDICAID+MENTHEALTH+NOSEATBELT+SMOKER+
                      EMPHYSEMA+STROKE+CORONARY+CHOLEST+CANCER+ASTHMA+HIGHBP,
                    data=meps, 
                    importance=TRUE, 
                    ntree=2000)

# Plot results by variable:
# 1) MeanDecreaseAccuracy plot: how much model fit decreases when dropping that column
# 2) MeanDecreaseGini plot: explanatory power of that column
# Use MeanDecreaseAccuracy for variable selection
varImpPlot(fit)

# Example 2.2: Generalized Linear Model (Logistic Regression)
# https://www.r-bloggers.com/roc-curves-and-classification/
# Run regression with best 4 variables using random forest MeanDecreaseAccuracy results
reg=glm(greater10K~CORONARY+UNINSURED+MEDICAID+AGE,data=meps,family=binomial(link='logit'))

# Convert regression into a probability response of the event occurring (i.e. that 
# the individual will incur expenses above 10K) 
S=predict(reg,type="response")

# define the 10K occurence variable as generic variable Y for the function below
Y=meps$greater10K

# Define function which ingests a user-specified threshold cutoff between 0 and 1,
# and outputs the lift value. The calculation includes determining true positives, 
# false positives
roc.curve=function(s,print=FALSE){
  Ps=(S>s)*1
  FP=sum((Ps==1)*(Y==0))/sum(Y==0)
  TP=sum((Ps==1)*(Y==1))/sum(Y==1)
  if(print==TRUE){
    print(table(Observed=Y,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}

# Allow the ROC value to be calculated a sequence of times
ROC.curve=Vectorize(roc.curve)

# Calculate the ROC values in a sequence between 0 and 1 in increments of 0.01
M.ROC=ROC.curve(seq(0,1,by=0.01))
# Plot the ROC results to show the lift
plot(M.ROC[1,],M.ROC[2,],col="grey",lwd=2,type="l")

# Example 2.3: Classification Tree
# https://www.r-bloggers.com/roc-curves-and-classification/
# Perform a similar exercise to the logisitic regression above, but use a classification tree instead

library(tree)
# Compute the classification tree based on input variables and data=meps
ctr <- tree(meps$greater10K~CORONARY+UNINSURED+MEDICAID+AGE, data=meps)
# Plot the skeleton of the tree
plot(ctr)
# Add the text to the tree
text(ctr)
# Compute the probabilities
S=predict(ctr)

# Compute the ROC curve for tree
M.ROC.tree=ROC.curve(seq(0,1,by=0.01))

# Plot the logistic regression ROC curve, with the tree ROC curve overlayed in gray for comparison
plot(M.ROC[1,],M.ROC[2,],type="l")
lines(M.ROC.tree[1,],M.ROC.tree[2,],type="l",col="grey",lwd=2)




################################################################################################

# CASE STUDY 3: Predict expenditure value for an individual
# METHOD: Use decision trees and then prune the tree to improve performance

library(rpart)

# Example 3.1: Simple example of a decision tree with only a few explanatory variables
results <- rpart(EXPEND ~ INCOME + AGE + BMI, method="anova", data=meps)
summary(results)
# Use CP to do cross validation -- CP determines the level of complexity
# Tradeoff between bias and variance
plotcp(results)
# plot tree 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(results) # visualize cross-validation results   
# Visualize tree
par(mfrow=c(1,1))
plot(results, uniform=TRUE, 
     main="Regression Tree for Expenditures ")
text(results, use.n=TRUE, all=TRUE, cex=0.8)

# Example 3.2: Full decision tree methodology
# Split train and holdout samples
train = meps[meps$PANEL==13,]
holdout = meps[meps$PANEL==14,]
# Fit a decision tree
# Note the control option fits with a very low cp value (overfit)
results <- rpart(log(EXPEND) ~ log(INCOME)+AGE+BMI+HISPANIC+EMPLOYED+UNINSURED+MEDICAID+MENTHEALTH+NOSEATBELT
                 +SMOKER+EMPHYSEMA+STROKE+CORONARY+CHOLEST+CANCER+ASTHMA+HIGHBP, method="anova", data=train,control=list(cp=.001))
summary(results)
plotcp(results)
# As size gets larger so does CV error
# plot tree 
plot(results, uniform=TRUE, 
     main="Regression Tree for Expenditures ")
text(results, use.n=TRUE, all=TRUE, cex=0.8)
# Overly complex!

# Example 3.3: Trim the tree
# Select the complexity which minimizes the error
# This is known as "Pruning"
pfit <- prune(results, cp=results$cptable[which.min(results$cptable[,"xerror"]),"CP"])
plot(pfit, uniform=TRUE, 
     main="Regression Tree for Expenditures ")
text(pfit, use.n=TRUE, all=TRUE, cex=0.8)
# Tree size is much more manageable

# Compute error on training sample
sum((predict(pfit,train)-train$LOGEXPEND)^2) # Pruned to not overfit
sum((predict(results,train)-train$LOGEXPEND)^2) # Overfit
# Note that the full model performs very well on the training sample

# Compute error on holdout sample
sum((predict(pfit,holdout)-holdout$LOGEXPEND)^2) # Pruned to not overfit
sum((predict(results,holdout)-holdout$LOGEXPEND)^2) # Overfit
# The full model vastly overfits because its performance is much worse on the holdout





