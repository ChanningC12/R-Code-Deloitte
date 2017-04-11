library(dplyr)
library(plyr)
################# QC Scoring ########################
NFO_score = read.csv("../../../QC_NFO_0207.csv")
RBO_score = read.csv("../../../QC_RBO_0207.csv")
str(NFO_score)
str(RBO_score)

# Create diff variables
NFO_score$Diff = NFO_score$PROB_NFO - NFO_score$NFO_1_0126_02_JMP
RBO_score$Diff = RBO_score$PROB_RBO - RBO_score$Prob_RBO_JMP

# Beyond 3 digit indicator
NFO_score$ind = ifelse(NFO_score$Diff>=0.001,1,0)
RBO_score$ind = ifelse(RBO_score$Diff>=0.001,1,0)
NFO_ind = count(NFO_score$ind)
RBO_ind = count(RBO_score$ind)
NFO_ind$Desc = ifelse(NFO_ind$x==0,"Diff<0.001","Diff>=0.001")
RBO_ind$Desc = ifelse(RBO_ind$x==0,"Diff<0.001","Diff>=0.001")
NFO_ind$Percentage = NFO_ind$freq / sum(NFO_ind$freq)
RBO_ind$Percentage = RBO_ind$freq / sum(RBO_ind$freq)
NFO_ind_new = NFO_ind[,c(3,2,4)]
RBO_ind_new = RBO_ind[,c(3,2,4)]

# Summary of Difference in Scoring
summary(NFO_score$Diff,digits = 3)
summary(RBO_score$Diff,digits = 3)

# Histogram plot
hist(NFO_score$Diff,col="grey",breaks = 100,main="NFO Scoring Differennce")
hist(RBO_score$Diff,col="grey",breaks = 100,main="RBO Scoring Differennce")

# Boxplot
boxplot(NFO_score$Diff,main="NFO Scoring Difference")
boxplot(RBO_score$Diff,main="RBO Scoring Difference")


