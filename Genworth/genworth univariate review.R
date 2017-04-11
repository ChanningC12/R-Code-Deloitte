# read in selected variables from Genworth
gen_corr = read.csv("../Desktop/CORR.csv")
str(gen_corr)
sapply(gen_corr, function(x) sum(is.na(x)))

# generate a list of variables with correlation
corr = cor(gen_corr[,4:64],gen_corr[1:3],use="pairwise.complete.obs")
corr = as.data.frame(corr)

# correlation plot
corrplot(corr)

# indicate + -
corr$FULL_PAY_SIGN = ifelse(corr$FULL_PAY_IND>0,1,-1)
corr$RBO_SIGN = ifelse(corr$RBO_IND>0,1,-1)
corr$NFO_SIGN = ifelse(corr$NFO_IND>0,1,-1)

# generate absolute correlation coeeficient
corr$FULL_PAY_IND_ABS = abs(corr$FULL_PAY_IND)
corr$RBO_IND_ABS = abs(corr$RBO_IND)
corr$NFO_IND_ABS = abs(corr$NFO_IND)

# Rank by FULL PAY
corr_FP = corr[order(-corr$FULL_PAY_IND_ABS),]
corr_RBO = corr[order(-corr$RBO_IND_ABS),]
corr_NFO = corr[order(-corr$NFO_IND_ABS),]

# Random Forest VarImp
library(randomForest)
library(caret)
library(MASS)
data("Boston")
help(Boston)

bag.boston = randomForest(medv~.,data=Boston,mtry=6,importance = T)
bag.boston
varImp(bag.boston)





