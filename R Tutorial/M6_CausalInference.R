###############################################
## R: INTRODUCTION TO CAUSAL INFERENCE 2016 ##
###############################################

cat("\014")
rm(list=ls())

####################################################################################

makeHists <- function(bins, covVec, treatVec, bigCovVec, bigTreatVec,nb,nbs) {
  
  xlim.<-c(min(covVec),max(covVec))
  
  par(mfcol=c(2,max(bins)+1))
  
  hist(bigCovVec[bigTreatVec==1],main="Original Treated",
       xlab="",ylab="",xlim=xlim.,breaks=nb)
  hist(bigCovVec[bigTreatVec==0],main="Original Control",
       xlab="",ylab="",xlim=xlim.,breaks=10*nb)
  
  for (ii in 1:max(bins)) {
    
    xlim.<-c(min(covVec[bins==ii]),max(covVec[bins==ii]))
    hist(covVec[treatVec==1 & bins==ii],
         main=paste("Treated: Subclass ",ii,sep=""),
         xlab="",ylab="",xlim=xlim.,breaks=nbs)
    hist(covVec[treatVec==0 & bins==ii],
         main=paste("Control: Subclass ",ii,sep=""),
         xlab="",ylab="",xlim=xlim.,breaks=nbs)		
    
  }
  
  
}

####################################################################################

#### Read in the NSW experimental dataset

filepath <- "C:\\Users\\data\\"
exp <- read.table(paste(filepath,"nsw.txt",sep=""),header=TRUE)

# take "TYPE" out (all are EXP)
exp <-exp[,-13]

dim(exp)
head(exp)

#### Assess initial covariate (in)balance

exp_X0_mean <- apply(exp[exp$TREAT==0,],2,mean)[3:12]
exp_X1_mean <- apply(exp[exp$TREAT==1,],2,mean)[3:12]
exp_mean_diff <- exp_X1_mean - exp_X0_mean


exp_t_test <- c()
for (cov in 2:11) {
  exp_t_test <- c(exp_t_test,
                   t.test(exp[exp$TREAT==1,cov],exp[exp$TREAT==0,cov])$p.value)
}

exp_balance <- cbind(exp_X1_mean, exp_X0_mean, exp_mean_diff, exp_t_test)
round(exp_balance, 2)


#### get causal estimate of average causal effect estimand

expNT <- sum(exp$TREAT)
expNC <- nrow(exp)-expNT
expDiff <- mean(exp$RE78[exp$TREAT==1]) - mean(exp$RE78[exp$TREAT==0])

expVarEst <- var(exp$RE78[exp$TREAT==1])/expNT + var(exp$RE78[exp$TREAT==0])/expNC

expLB1 <- expDiff-1.96*sqrt(expVarEst)
expUB1 <- expDiff+1.96*sqrt(expVarEst)

expDiff
round(c(expLB1, expUB1))

#### now using regression (OK)

exp_reg <- lm(RE78~., data=exp)
summary(exp_reg)
confint(exp_reg)

####################################################################################

#### Read in Lalonde dataset

filepath <- "C:\\Users\\data\\"
obs <- read.table(paste(filepath,"lalonde.txt",sep=""),header=TRUE)

head(obs)

#### Temporarily remove the outcome, RE78, from the data set

obs<-obs[,-1]
# also take "TYPE" out
obs<-obs[,-12]

head(obs)

#### Assess initial covariate (in)balance

X0_mean <- apply(obs[obs$TREAT==0,],2,mean)[2:11]
X1_mean <- apply(obs[obs$TREAT==1,],2,mean)[2:11]
orig_mean_diff <- X1_mean - X0_mean


orig_t_test <- c()
for (cov in 2:11) {
  orig_t_test <- c(orig_t_test,
                    t.test(obs[obs$TREAT==1,cov],obs[obs$TREAT==0,cov])$p.value)
}

orig_balance <- cbind(X1_mean, X0_mean, orig_mean_diff, orig_t_test)
round(orig_balance, 2)


#### Now build a propensity score model

mod <- glm(TREAT~., data=obs, family=binomial(link=logit))
summary(mod)
pscores <- mod$fitted.values

# box plot of propensity scores -- comment
dev.new()
dev.new()
boxplot(pscores[obs$TREAT==1],pscores[obs$TREAT==0],
		names=c("Treatment Group","Control Group"),main="Estimated Propensity Scores")

# histogram of fitted proensity scores -- comment
dev.new()
par(mfrow=c(2,1),mai=c(0.5,0.5,0.5,0.5))
hist(pscores[obs$TREAT==1],xlim=c(0,1),xlab="propensity score",
	 main ="Active Treatment",col="grey",border="white")
hist(pscores[obs$TREAT==0],xlim=c(0,1), xlab="propensity score",
	 main="Control Treatment",col="grey",border="white")

#### propensity-score based trimming

## discard control units with estimated p-scores lower than min of active treated units'
## estimated p-scores, or higher than max of active treatment units' estimated p-scores

cut <- min(pscores[obs$TREAT==1])
obs2 <- obs[pscores>=cut, ]

dim(obs)
nrow(obs) - nrow(obs2)   # number discarded
nrow(obs2)               # number left

## how would you justify discarding these units to a client?

pscores2 <- pscores[pscores>=cut]


## plot new p-scores
dev.new()
par(mfrow=c(1,1))
boxplot(pscores2[obs2$TREAT==1], pscores2[obs2$TREAT==0])


#### create 5 subclasses based on quintiles of estimated p-score -- comment
#### generally, can get rid of 80-90% of bias based on quintile p-score sub-classification

breaks <- quantile(pscores2, c(.2,.4,.6,.8))

bins <- rep(NA,nrow(obs2))
bins[pscores2<=breaks[1]] <- 1
bins[pscores2>breaks[1] & pscores2<=breaks[2]] <- 2
bins[pscores2>breaks[2] & pscores2<=breaks[3]] <- 3
bins[pscores2>breaks[3] & pscores2<=breaks[4]] <- 4
bins[pscores2>breaks[4]] <- 5

table(bins, obs2$TREAT)


## Now try with quantiles (0.7, 0.8, 0.9, 0.95) -- comment

breaks <- quantile(pscores2, c(.7, .8, .9, .95))

bins <- rep(NA,nrow(obs2))
bins[pscores2<=breaks[1]] <- 1
bins[pscores2>breaks[1] & pscores2<=breaks[2]] <- 2
bins[pscores2>breaks[2] & pscores2<=breaks[3]] <- 3
bins[pscores2>breaks[3] & pscores2<=breaks[4]] <- 4
bins[pscores2>breaks[4]] <- 5

thetab <- table(bins, obs2$TREAT)
thetab
treatweights <- thetab[,2]

#### for each subclass above, compute mean within each treatment group for each covariate
#### descibe what you see

meanT <- matrix(NA,nrow=max(bins),ncol=10)
rownames(meanT) <- 1:5
colnames(meanT) <- colnames(obs)[2:11]
meanC <- meanT

#### summarize average balance achieved for each covariate by averaging the difference
#### in means accross subclassess, weighting by the number of units in the ACTIVE
#### treatment group (why?) -- comment

for (bin in 1:5) {
	for (cov in colnames(meanT)) {
		meanT[bin,cov] <- mean(obs2[bins==bin & obs2$TREAT==1,cov])
		meanC[bin,cov] <- mean(obs2[bins==bin & obs2$TREAT==0,cov])
	}
}

weights <- treatweights/sum(treatweights)


subclass_balance <- cbind(round(t(meanT)%*%weights, 2),
                    round(t(meanC)%*%weights, 2))
colnames(subclass_balance) <- c("X1_mean", "X0_mean")
round(subclass_balance, 2)
 
#### visually display covariate balance

## Re75 variable

dev.new()
makeHists(bins, obs2$RE75, obs2$TREAT, obs$RE75, obs$TREAT, 10, 12)

#######################################################################################


#### now read in outcome again
obs <- read.table(paste(filepath,"lalonde.txt",sep=""),header=TRUE)
# for ease of coding, take "TYPE" out
obs <- obs[,-13]

#### naively, pretending the study was a randomized control trial,
#### estimate average treatment effect

obsNT <- sum(obs$TREAT)
obsNC <- nrow(obs)-obsNT

obsDiff <- mean(obs$RE78[obs$TREAT==1]) - mean(obs$RE78[obs$TREAT==0])

obsVarEst <- var(obs$RE78[obs$TREAT==1])/obsNT + var(obs$RE78[obs$TREAT==0])/obsNC

obsLB1 <- obsDiff-1.96*sqrt(obsVarEst)
obsUB1 <- obsDiff+1.96*sqrt(obsVarEst)

round(obsDiff)
round(c(obsLB1, obsUB1))

#### now try regression! -- comment

reg <- lm(RE78~., data=obs)
summary(reg)
confint(reg)

#### now try naive t-test based estimate on trimmed dataset -- comment

cut <- min(pscores[obs$TREAT==1])
obs2 <- obs[pscores>=cut,]

pointest <- mean(obs2$RE78[obs2$TREAT==1])-mean(obs2$RE78[obs2$TREAT==0])
varest <- var(obs2$RE78[obs2$TREAT==1])/length(obs2$RE78[obs2$TREAT==1]) +
		      var(obs2$RE78[obs2$TREAT==0])/length(obs2$RE78[obs2$TREAT==0])

lb <- pointest - 1.96*sqrt(varest)
ub <- pointest + 1.96*sqrt(varest)

round(pointest)
round(c(lb, ub))

# now try regression on trimmed dataset -- comment

regout <- lm(RE78~., data=obs2)
summary(regout)
confint(regout)

#### finally, obtain t-test based estimate of average treatment effect, weighted according
#### to the number of units in the ACTIVE treatment group in each subclass

diff <- rep(NA,5)
vars <- rep(NA,5)

for (b in 1:5) {
	
	temp <- obs2[bins==b,]
	diff[b] <- mean(temp$RE78[temp$TREAT==1])-mean(temp$RE78[temp$TREAT==0])
	vars[b] <- var(temp$RE78[temp$TREAT==1])/length(temp$TREAT[temp$TREAT==1]) +
			       var(temp$RE78[temp$TREAT==0])/length(temp$TREAT[temp$TREAT==0])
	
}

pointest <- diff%*%weights
varest <- vars%*%weights^2

lb <- pointest-1.96*sqrt(varest)
ub <- pointest+1.96*sqrt(varest)

round(as.numeric(pointest))
c(lb, ub)

pointest


