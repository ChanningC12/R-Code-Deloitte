library(dplyr)
library(plyr)
library(ggplot2)
################# QC Scoring ########################
FULLPAY = read.csv("../../../Probability Comparison_FullPay_0213.csv")

# Beyond 3 digit indicator
FULLPAY$ind = ifelse(FULLPAY$PROB_DIFF>=0.001,1,0)
FULLPAY_ind = count(FULLPAY$ind)
FULLPAY_ind$Desc = ifelse(FULLPAY_ind$x==0,"Diff<0.001","Diff>=0.001")
FULLPAY_ind$Percentage = FULLPAY_ind$freq / sum(FULLPAY_ind$freq)
FULLPAY_ind_new = FULLPAY_ind[,c(3,2,4)]

FULLPAY$ind = ifelse(FULLPAY$PROB_DIFF_ABS>=0.001,1,0)
FULLPAY_ind = count(FULLPAY$ind)
FULLPAY_ind$Desc = ifelse(FULLPAY_ind$x==0,"Diff<0.001","Diff>=0.001")
FULLPAY_ind$Percentage = FULLPAY_ind$freq / sum(FULLPAY_ind$freq)
FULLPAY_ind_new = FULLPAY_ind[,c(3,2,4)]


# Summary of Difference in Scoring
summary(FULLPAY$PROB_DIFF,digits = 3)
summary(FULLPAY$PROB_DIFF_ABS,digits = 3)

# Histogram plot
hist(FULLPAY$PROB_DIFF,col="grey",breaks = 100,main="Full Pay Scoring Differennce")
hist(FULLPAY$PROB_DIFF_ABS,col="grey",breaks = 100,main="Full Pay Scoring Differennce")

# Boxplot
boxplot(FULLPAY$PROB_DIFF,main="Full Pay Scoring Difference")
boxplot(FULLPAY$PROB_DIFF_ABS,main="Full Pay Scoring Difference")

