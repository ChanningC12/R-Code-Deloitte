######################################################################
#	program:	MEPS regression study.R
#	authors:	Jim Guszcza, Andrew Vesper Deloitte Consulting
#	date:		September 23, 2016
#	Regression case study - estimate medical expenditures
#
#	Note - data used in the chapter "Linear Models" (by Marjorie Rosenberg
#	and James Guszcza) of the forthcoming essay collection edted
#	by Frees, Meyers and Derrig
#
# For use in internal R training materials 
################################################################################################

# Illustrative of process of the step-by-step model building for client projects
#	Focus on iterative modeling process and model validation
# Linear Regression

# Remove all objects from the R session
# Create a clean slate to start modeling
rm(list = ls()) 
# Load the MASS package which contains useful functions that are not in base R
library(MASS)

######################################################################
# 	Part 0: Load, audit, discuss data
######################################################################

# Set working directory to read data
# Commands referencing a filename will search in this directory
setwd("C:\\Users\\avesper\\Desktop\\R Course Material\\Data\\")
# Read in data by referencing the file name
meps <- read.csv("MEPS.Diabetes.SOA.csv")
# The data is read in as a Data Frame
# Quick data summary for each variable
summary(meps)
# Dimensions of the data matrix
dim(meps)
# Look at the first 5 rows of data
head(meps)
	

######################################################################
# Part 1: Univariate Data Exploration, Variable Transformations
######################################################################

# Is EXPEND roughly lognormal?
# Create a histogram of the data
# xlab option labels the x-axis, main option labels the title
truehist(meps$EXPEND, xlab="Expend",main='EXPEND Distribution', col='lavender',col.main='navy') 

# Create the target variable as log(EXPEND)
Y <- log(meps$EXPEND)
# Plot histogram (this time using base R)
hist(Y,col='lavender',freq=F)
# Compare to Normal distribution
# dnorm is the Normal density, add = T adds the curve to the histogram
curve(dnorm(x,mean=mean(Y),sd=sd(Y)),add=T,yaxt='n',col='dark blue',lwd=2)

# Let's add a kernel density estimate because it's cool
# This adds a non-parametric kernel density to the plot
lines(density(Y), col='red', lwd='2')
# Close match with the Normal distribution

# Assess the Normality of our transformed income variable Y
# Normal probability plot
qqnorm(Y)
qqline(Y, col='blue', lwd=2)
# Maybe slightly right-skewed

# While we're at it, also look at the income variable
hist(meps$INCOME, xlab="Income", col='lavender',main='')
hist(log(meps$INCOME), xlab="Log(Income)", col='lavender',main='')
# Also useful for the log transformation on our 'X' variables
meps$LOGINCOME <- log(meps$INCOME)
meps$LOGEXPEND <- log(meps$EXPEND)

# Consider the values of RACE.RAW
# Table is a crosstabulation function
table(meps$RACE.RAW)
# There are 6 race categories, most of which are not well populated
# We probably want to simplify
race <- meps$RACE.RAW
lev <- levels(race)
lev
# race is a factor variable, which is labeled by "levels"
# We can combine levels 3-6 into a single level labeled "3 other"
lev[3:6] <- "3 other"
levels(race) <- lev 
levels(race)
# The race variable combines amerind, asian, hawaiian, and mult into a single group labeled other
# Consider a cross tabulation of the original RACE.RAW and the new race variable
table(meps$RACE.RAW, race)
meps$RACE <- race
# For regression models we typically convert categorical variables to dummy variables
# Create indicator variables for BLACK and OTHER
# WHITE is the reference group
meps$BLACK <- I(meps$RACE=='2 black')*1
meps$OTHER <- I(meps$RACE=='3 other')*1
table(meps$OTHER,meps$RACE.RAW)

# What about the SMOKER variable?
table(meps$SMOKER)
# Lots of missingness (coded as 99)
# How to handle this
# Consider boxplots of LOGEXPEND for different SMOKER statuses
boxplot(LOGEXPEND~SMOKER,data=meps)
# How would you handle the missing data?
# Discuss
# For this model we will not use the SMOKER variable, so we keep the rows with missing smoker data

# After completing all variable transformations
#	Split data into training, holdout sets
#	at this time, explain the concept of cross-validation
#	(see slides for brief theoretical discussion)
# This subsets the data set by PANEL, train on Panel 13, test on Panel 14
train   <- meps[meps$PANEL==13,]
holdout <- meps[meps$PANEL==14,]
nrow(meps); nrow(train); nrow(holdout)

# reality check - do expendituares look consistent across panels?
boxplot(LOGEXPEND~PANEL, data=meps, col="lavender", yax="n")

# another way of seeing the same thing
qqplot(train$EXPEND, holdout$EXPEND, log='xy')
abline(0,1, col='blue', lwd=2)
# This compares the distribution of expenditures in the samples


######################################################################
# Part 2: EAD
######################################################################

# Typical model building would proceed from univariate analysis
# to some EAD and bivariate analysis
# Refer to previous modules for this material


######################################################################
# Part 3: Iterative Modeling Process
######################################################################

# GOAL: build a predictive model for LOGEXPEND
# let's start with a simple model and build up

# lm is the command for linear regression in R
# The linear regression equation y ~ x using the training data
# Store the results of the regression as the object r10 
r10 <- lm(LOGEXPEND~LOGINCOME, data=train)
# What is contained in the r10 object
names(r10)
# Can use the $ to access the elements of r10
r10$coefficients
# There is also a summary object for the linear regression
summary(r10)
# How would you interpret this output?
names(summary(r10))
summary(r10)$sigma
summary(r10)$r.squared

# Residuals vs fitted values plot
plot(r10$residuals~r10$fitted.values,xlab='Fitted Values',ylab='Residuals')
# Add a horizontal line at 0
abline(h=0)

# QQ plot of residuals
qqnorm(r10$residuals)
qqline(r10$residuals)

# The r10 object also has a plot function
# Diagnostic plots for linear regression
plot(r10)

# The update function takes the r10 regression and adds additional variables
# The new model is stored as object r10
# This is equivalent to: r11 <- lm(LOGEXPEND~LOGINCOME+AGE,data=train)
r11 <- update(r10, .~. + AGE)
summary(r11)
# LOGINCOME is borderline significant int he presence of AGE

# Add the BMI variable
r12 <- update(r11, .~. + BMI)
summary(r12)
# LOGINCOME no longer significant in the presence of AGE and BMI

r13 <- update(r12, .~. + UNINSURED)
summary(r13)

# LOGINCOME becomes significant again in the presence of UNINSURED!
boxplot(INCOME~UNINSURED, data=train, log='y')
# UNINSURED individuals have much lower income
# Controlling for insurance status, income is significant
# Because INCOME and UNINSURED are correlated, the effect of INCOME is confounded by UNINSURED

# r14 drops LOGINCOME from the model
r14 <- update(r13, .~. - LOGINCOME)
summary(r14)
# Compare coefficients
coef(r13); coef(r14)

# Use different criterion for model comparison
# Is LOGINCOME signicant -- can use t-test but consider other measures
# F-test
anova(r13,r14)
# AIC and BIC information criterion, smaller is better!
# Penalize models for the number of parameters
AIC(r13,r14)
BIC(r13,r14)
# let's keep LOGINCOME in the model
# Note that AIC and BIC give different results (BIC has a stronger penalty for additional params)

# Add HISPANIC
r15 <- update(r13, .~. + HISPANIC)
summary(r15)

# What about gender
r16 <- update(r15, .~. + FEMALE)
summary(r16)
# Not significant

# maybe gender is significant in an interaction with AGE
# Note -- AGE:FEMALE is an interaction term
r17 <- update(r15, .~. + FEMALE + AGE:FEMALE)
summary(r17)
# nope

# test MENTHEALTH
# (review types of variables - continuous, binary, ordinal, nominal)
# The factor command in R treats MENTHEALTH as a categorical variable (nominal)
r18 <- update(r15, .~. + factor(MENTHEALTH))
summary(r18)
# The model automatically selects MENTHEALTH=1 as the reference level
# Note: at this point LOGINCOME is no longer 0.05 significant -- leave it in the regression
# Note that MENTHEALTH 2 is not significant -- how would you handle this?

# Side conversation - let's talk about the magnitude of these effects
# Difference between statistical and economic significance
# "Thrive" by Layard and Clark reports that of the people with chronic
# diseases, those who also have mental health problems have ~50% higher
# healthcare utilization than those who do not

# back to the show	
# test MEDICAID
r19 <- update(r18, .~. + MEDICAID)
summary(r19)

# why is MEDICAID not significant?
boxplot(LOGEXPEND ~ MEDICAID, train, log='y')
summary(lm(LOGEXPEND~MEDICAID, data=train))
# (discuss collinearity)
# People on medicaid are not uninsured
# People on medicaid have lower income
boxplot(LOGINCOME ~ MEDICAID, train, log='y')
plot(table(train$MEDICAID, train$UNINSURED), col=3:4)
# When accounting for LOGINCOME and UNINSURED, MEDICAID does not add predictive value	
# leave MEDICAID out

# test RACE
r20 <- update(r18, .~. + RACE)
summary(r20)

# test EMPLOYED
r21 <- update(r20, .~. + EMPLOYED)
summary(r21)
# Is the sign of the coefficient on EMPLOYED surprising?

# We have included main affects, what about interactions and squared terms
# test for some nonlinearities and interactions
# In R formulas, x:y is an interaction
# I(x^2) creates a squared variable
summary(update(r21, .~. + AGE:EMPLOYED))
summary(update(r21, .~. + AGE:UNINSURED))
summary(update(r21, .~. + AGE:HISPANIC))
summary(update(r21, .~. + I(AGE^2)))
summary(update(r21, .~. + I(BMI^2)))
summary(update(r21, .~. + I(LOGINCOME^2)))
# Nothing that convincing

# now test comorbidity variables
summary(r21)
r22 <- update(r21, .~. +  CORONARY + CANCER + HIGHBP + EMPHYSEMA + ASTHMA + CHOLEST + STROKE)
summary(r22)
anova(r22, r21, test="F")
AIC(r22, r21)
BIC(r22, r21)
# Adding comorbidity significantly improves performance

# Let's treat model r23 as our "final" model
mmod <- r22
# discuss how to calculate predicted values
# predict command in R computes fitted values for the data 
# (you can optionally input newdata into the predict function)
preds <- predict(mmod)
# Alternatively, we can calculate by hand
# Define Y as LOGEXPEND
Y <- train$LOGEXPEND
# Store regression coefficients as Beta
beta <- coef(mmod)
X <- model.matrix(mmod)     # design matrix
head(X)
# %*% is matrix multiplication in R
eta <- X %*% beta
eta <- as.vector(eta)
summary(preds - eta)
# Same

# A quick note on the convexity of the log transformation
mean(Y); mean(eta)			# what we expect
mean(exp(Y)); mean(exp(eta))		# hmmmm...
# Convexity correction
median(exp(Y)); median(exp(eta))
# Predicts median EXPEND but not mean EXPEND

# Lastly, consider a stepwise regression
# Above we did a sort of manual version
# step command uses AIC criterion to do stepwise regression

# Start with the full model including all variables
# Ignore interactions and higher order terms
mod.full <- lm(LOGEXPEND~LOGINCOME+AGE+BMI+FEMALE+HISPANIC+EMPLOYED+UNINSURED+MEDICAID+as.factor(MENTHEALTH)+NOSEATBELT+EMPHYSEMA+STROKE+CORONARY+CHOLEST+CANCER+ASTHMA+HIGHBP+BLACK+OTHER,data=train)
summary(mod.full)
# Stepwise using the full model as the starting point               
mod.step <- step(mod.full,data=train)
summary(mod.step)
# How does this compare to our final model
AIC(mmod,mod.step)
# Very similar, maybe slightly more parsimonious
# Let's use our "expert" judgment and stick with mmod as our best model
summary(mmod)
plot(mmod)


######################################################################
# 	Part 4: Model Validation
#	up till now, the holdout data has been held in cold storage
######################################################################

# discuss cross-validation
# For now, just do a simple holdout comparison
# We have trained the models on Panel 13
# Compare model prediction performance on Panel 14

# Compare mmod to mod.step to mod.full

### MAD comparison

# For the full model
# Adjusted R-Squared
summary(mod.full)$adj
# Determine holdout sample "actual" value
y.test <- holdout$LOGEXPEND
# Create holdout sample "predicted" value
# Note that the predict command takes the holdout data as input to make predictions
yhat.test.full <- predict(mod.full, holdout, type="response")
# Compute MAD
mean(abs(y.test-yhat.test.full))

# Repeat for our "best" model
summary(mmod)$adj
y.test <- holdout$LOGEXPEND
yhat.test.mmod <- predict(mmod, holdout, type="response")
mean(abs(y.test-yhat.test.mmod))

# Repeat for the stepwise model
summary(mod.step)$adj
y.test <- holdout$LOGEXPEND
yhat.test.step <- predict(mod.step, holdout, type="response")
mean(abs(y.test-yhat.test.step))

# This is arguably more relevant for machine learning types of algorithms
# Full model actually has lowest MAD on the test data, but similar performance
# mod.step is pretty similar to mod.full


