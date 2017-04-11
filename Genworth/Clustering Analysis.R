library(lattice)

pattern = read.csv("../../../SOM.csv")
pattern$EFF_DT=as.Date(pattern$EFF_DT,"%d%b%Y")
str(pattern)

cm$eff_dt = as.Date(cm$eff_dt,"%m/%d/%Y")
str(cm)

pattern_0221 = merge(pattern,cm,by.x=c("PLCY_REF","EFF_DT"),by.y=c("plcy_ref","eff_dt"),all.y=T)

# Histogram
histogram(~ RATE_INCR_UC|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))
histogram(~ TOTAL_COMM_INCR_RATE_UC|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))
histogram(~ RATE_INCR_AFFORD|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))
histogram(~ ISSUE_AGE|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))


# K-means
km = kmeans(pattern_0221[,c(3:23)],12,nstart=10,iter.max=50)
km
pattern_0221$cluster = km$cluster

prop.table(table(pattern_0221$RBO_CM))
prop.table(table(pattern_0221$RBO_CM,pattern_0221$cluster),2)

str(km)
table(pattern_0221$cluster)

# k = 6
set.seed(6666)
km = kmeans(pattern_0221[,c(4:9,12,15,17,19:23)],6,nstart=10,iter.max=50)
km
pattern_0221$cluster = km$cluster

prop.table(table(pattern_0221$RBO_CM))
prop.table(table(pattern_0221$RBO_CM,pattern_0221$cluster),2)

prop.table(table(pattern_0221$NFO_CM))
prop.table(table(pattern_0221$NFO_CM,pattern_0221$cluster),2)

str(km)
table(pattern_0221$cluster)


# k = 4
set.seed(4444)
km = kmeans(pattern_0221[,c(4:9,12,15,17,19:23)],4,nstart=10,iter.max=50)
km
pattern_0221$cluster = km$cluster

prop.table(table(pattern_0221$RBO_CM))
prop.table(table(pattern_0221$RBO_CM,pattern_0221$cluster),2)

prop.table(table(pattern_0221$NFO_CM))
prop.table(table(pattern_0221$NFO_CM,pattern_0221$cluster),2)

str(km)
table(pattern_0221$cluster)

library(cluster)
library(fpc)
# plot first two principal components
plotcluster(pattern_0221[,c(4:9,12,15,17,19:23)], km$cluster,pch=pattern_0221$RBO_CM)
clusplot(pattern_0221[,c(4:9,12,15,17,19:23)], km$cluster)
pca = prcomp(pattern_0221[,c(4:9,12,15,17,19:23)],center = TRUE,
             scale. = TRUE)
pca
summary(pca)

# Summary Table
ANNL_PREM_BFOR_RBO <- pattern_0221 %>%
  group_by(RBO_CM)%>%
  dplyr::summarise(mean=mean(ANNL_PREM_BFOR),
                   Low_10 = quantile(ANNL_PREM_BFOR,0.1),
                   Quar_1 = quantile(ANNL_PREM_BFOR,0.25),
                   median = median(ANNL_PREM_BFOR),
                   Quar_3 = quantile(ANNL_PREM_BFOR,0.75),
                   High_10 = quantile(ANNL_PREM_BFOR,0.9))
histogram(~ ANNL_PREM_BFOR|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

ANNL_PREM_BFOR_NFO <- pattern_0221 %>%
  group_by(NFO_CM)%>%
  dplyr::summarise(mean=mean(ANNL_PREM_BFOR),
                   Low_10 = quantile(ANNL_PREM_BFOR,0.1),
                   Quar_1 = quantile(ANNL_PREM_BFOR,0.25),
                   median = median(ANNL_PREM_BFOR),
                   Quar_3 = quantile(ANNL_PREM_BFOR,0.75),
                   High_10 = quantile(ANNL_PREM_BFOR,0.9))
histogram(~ ANNL_PREM_BFOR|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))


############## Feb 22nd ################
# Change the benefit period logic
# BENE_TRM_BFOR
pattern_0221 = mutate(pattern_0221,
                      BENE_TRM_BFOR_NEW = ifelse(BENE_TRM_BFOR==0,6,BENE_TRM_BFOR))
with(pattern_0221,prop.table(table(RBO_CM,BENE_TRM_BFOR_NEW),1))
histogram(~ BENE_TRM_BFOR_NEW|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

with(pattern_0221,prop.table(table(NFO_CM,BENE_TRM_BFOR_NEW),1))
histogram(~ BENE_TRM_BFOR_NEW|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))
                   
# FPO count
with(pattern_0221,prop.table(table(RBO_CM,CNT_FPO_PREV1),1))
histogram(~ CNT_FPO_PREV1|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

with(pattern_0221,prop.table(table(NFO_CM,CNT_FPO_PREV1),1))
histogram(~ CNT_FPO_PREV1|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

# RBO count
with(pattern_0221,prop.table(table(RBO_CM,CNT_RBO_PREV1),1))
histogram(~ CNT_RBO_PREV1|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

with(pattern_0221,prop.table(table(NFO_CM,CNT_RBO_PREV1),1))
histogram(~ CNT_RBO_PREV1|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

# DECISION STAGE
with(pattern_0221,prop.table(table(RBO_CM,DECISION_STAGE),1))
histogram(~ DECISION_STAGE|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

with(pattern_0221,prop.table(table(NFO_CM,DECISION_STAGE),1))
histogram(~ DECISION_STAGE|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

# FAWP
with(pattern_0221,prop.table(table(RBO_CM,IFA_GROUP_STATE_PROD_FAWP),1))
histogram(~ IFA_GROUP_STATE_PROD_FAWP|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

with(pattern_0221,prop.table(table(NFO_CM,IFA_GROUP_STATE_PROD_FAWP),1))
histogram(~ IFA_GROUP_STATE_PROD_FAWP|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

# Health Component 1
HEALTH_COMPOSITE_1_RBO <- pattern_0221 %>%
  group_by(RBO_CM)%>%
  dplyr::summarise(mean=mean(HEALTH_COMPOSITE_1),
                   Low_10 = quantile(HEALTH_COMPOSITE_1,0.1),
                   Quar_1 = quantile(HEALTH_COMPOSITE_1,0.25),
                   median = median(HEALTH_COMPOSITE_1),
                   Quar_3 = quantile(HEALTH_COMPOSITE_1,0.75),
                   High_10 = quantile(HEALTH_COMPOSITE_1,0.9))
HEALTH_COMPOSITE_1_RBO = HEALTH_COMPOSITE_1_RBO[order(HEALTH_COMPOSITE_1_RBO$RBO_CM,decreasing = T),]
histogram(~ HEALTH_COMPOSITE_1|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

HEALTH_COMPOSITE_1_NFO <- pattern_0221 %>%
  group_by(NFO_CM)%>%
  dplyr::summarise(mean=mean(HEALTH_COMPOSITE_1),
                   Low_10 = quantile(HEALTH_COMPOSITE_1,0.1),
                   Quar_1 = quantile(HEALTH_COMPOSITE_1,0.25),
                   median = median(HEALTH_COMPOSITE_1),
                   Quar_3 = quantile(HEALTH_COMPOSITE_1,0.75),
                   High_10 = quantile(HEALTH_COMPOSITE_1,0.9))
HEALTH_COMPOSITE_1_NFO = HEALTH_COMPOSITE_1_NFO[order(HEALTH_COMPOSITE_1_NFO$NFO_CM,decreasing = T),]
histogram(~ HEALTH_COMPOSITE_1|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))


# HIGHCRD_BANKCARDCREDIT_NEW
HIGHCRD_BANKCARDCREDIT_NEW_RBO <- pattern_0221 %>%
  group_by(RBO_CM)%>%
  dplyr::summarise(mean=mean(HIGHCRD_BANKCARDCREDIT_NEW),
                   Low_10 = quantile(HIGHCRD_BANKCARDCREDIT_NEW,0.1),
                   Quar_1 = quantile(HIGHCRD_BANKCARDCREDIT_NEW,0.25),
                   median = median(HIGHCRD_BANKCARDCREDIT_NEW),
                   Quar_3 = quantile(HIGHCRD_BANKCARDCREDIT_NEW,0.75),
                   High_10 = quantile(HIGHCRD_BANKCARDCREDIT_NEW,0.9))
HIGHCRD_BANKCARDCREDIT_NEW_RBO = HIGHCRD_BANKCARDCREDIT_NEW_RBO[order(HIGHCRD_BANKCARDCREDIT_NEW_RBO$RBO_CM,decreasing = T),]
histogram(~ HIGHCRD_BANKCARDCREDIT_NEW|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

HIGHCRD_BANKCARDCREDIT_NEW_NFO <- pattern_0221 %>%
  group_by(NFO_CM)%>%
  dplyr::summarise(mean=mean(HIGHCRD_BANKCARDCREDIT_NEW),
                   Low_10 = quantile(HIGHCRD_BANKCARDCREDIT_NEW,0.1),
                   Quar_1 = quantile(HIGHCRD_BANKCARDCREDIT_NEW,0.25),
                   median = median(HIGHCRD_BANKCARDCREDIT_NEW),
                   Quar_3 = quantile(HIGHCRD_BANKCARDCREDIT_NEW,0.75),
                   High_10 = quantile(HIGHCRD_BANKCARDCREDIT_NEW,0.9))
HIGHCRD_BANKCARDCREDIT_NEW_NFO = HIGHCRD_BANKCARDCREDIT_NEW_NFO[order(HIGHCRD_BANKCARDCREDIT_NEW_NFO$NFO_CM,decreasing = T),]
histogram(~ HIGHCRD_BANKCARDCREDIT_NEW|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

# Spousal Discount
with(pattern_0221,prop.table(table(RBO_CM,IF_SPOUSE_DISC),1))
histogram(~ IF_SPOUSE_DISC|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

with(pattern_0221,prop.table(table(NFO_CM,IF_SPOUSE_DISC),1))
histogram(~ IF_SPOUSE_DISC|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

# NETW30
NETW30_RBO <- pattern_0221 %>%
  group_by(RBO_CM)%>%
  dplyr::summarise(mean=mean(NETW30),
                   Low_10 = quantile(NETW30,0.1),
                   Quar_1 = quantile(NETW30,0.25),
                   median = median(NETW30),
                   Quar_3 = quantile(NETW30,0.75),
                   High_10 = quantile(NETW30,0.9))
NETW30_RBO = NETW30_RBO[order(NETW30_RBO$RBO_CM,decreasing = T),]
histogram(~ NETW30|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

NETW30_NFO <- pattern_0221 %>%
  group_by(NFO_CM)%>%
  dplyr::summarise(mean=mean(NETW30),
                   Low_10 = quantile(NETW30,0.1),
                   Quar_1 = quantile(NETW30,0.25),
                   median = median(NETW30),
                   Quar_3 = quantile(NETW30,0.75),
                   High_10 = quantile(NETW30,0.9))
NETW30_NFO = NETW30_NFO[order(NETW30_NFO$NFO_CM,decreasing = T),]
histogram(~ NETW30|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

# ORIG_DBA
ORIG_DBA_BFOR_RBO <- pattern_0221 %>%
  group_by(RBO_CM)%>%
  dplyr::summarise(mean=mean(ORIG_DBA_BFOR),
                   Low_10 = quantile(ORIG_DBA_BFOR,0.1),
                   Quar_1 = quantile(ORIG_DBA_BFOR,0.25),
                   median = median(ORIG_DBA_BFOR),
                   Quar_3 = quantile(ORIG_DBA_BFOR,0.75),
                   High_10 = quantile(ORIG_DBA_BFOR,0.9))
ORIG_DBA_BFOR_RBO = ORIG_DBA_BFOR_RBO[order(ORIG_DBA_BFOR_RBO$RBO_CM,decreasing = T),]
histogram(~ ORIG_DBA_BFOR|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

ORIG_DBA_BFOR_NFO <- pattern_0221 %>%
  group_by(NFO_CM)%>%
  dplyr::summarise(mean=mean(ORIG_DBA_BFOR),
                   Low_10 = quantile(ORIG_DBA_BFOR,0.1),
                   Quar_1 = quantile(ORIG_DBA_BFOR,0.25),
                   median = median(ORIG_DBA_BFOR),
                   Quar_3 = quantile(ORIG_DBA_BFOR,0.75),
                   High_10 = quantile(ORIG_DBA_BFOR,0.9))
ORIG_DBA_BFOR_NFO = ORIG_DBA_BFOR_NFO[order(ORIG_DBA_BFOR_NFO$NFO_CM,decreasing = T),]
histogram(~ ORIG_DBA_BFOR|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

# PRCNT_NONMTGCREDIT
PRCNT_NONMTGCREDIT_RBO <- pattern_0221 %>%
  group_by(RBO_CM)%>%
  dplyr::summarise(mean=mean(PRCNT_NONMTGCREDIT),
                   Low_10 = quantile(PRCNT_NONMTGCREDIT,0.1),
                   Quar_1 = quantile(PRCNT_NONMTGCREDIT,0.25),
                   median = median(PRCNT_NONMTGCREDIT),
                   Quar_3 = quantile(PRCNT_NONMTGCREDIT,0.75),
                   High_10 = quantile(PRCNT_NONMTGCREDIT,0.9))
PRCNT_NONMTGCREDIT_RBO = PRCNT_NONMTGCREDIT_RBO[order(PRCNT_NONMTGCREDIT_RBO$RBO_CM,decreasing = T),]
histogram(~ PRCNT_NONMTGCREDIT|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

PRCNT_NONMTGCREDIT_NFO <- pattern_0221 %>%
  group_by(NFO_CM)%>%
  dplyr::summarise(mean=mean(PRCNT_NONMTGCREDIT),
                   Low_10 = quantile(PRCNT_NONMTGCREDIT,0.1),
                   Quar_1 = quantile(PRCNT_NONMTGCREDIT,0.25),
                   median = median(PRCNT_NONMTGCREDIT),
                   Quar_3 = quantile(PRCNT_NONMTGCREDIT,0.75),
                   High_10 = quantile(PRCNT_NONMTGCREDIT,0.9))
PRCNT_NONMTGCREDIT_NFO = PRCNT_NONMTGCREDIT_NFO[order(PRCNT_NONMTGCREDIT_NFO$NFO_CM,decreasing = T),]
histogram(~ PRCNT_NONMTGCREDIT|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))
                   
# ZAGG_PHEARTAT
ZAGG_PHEARTAT_RBO <- pattern_0221 %>%
  group_by(RBO_CM)%>%
  dplyr::summarise(mean=mean(ZAGG_PHEARTAT),
                   Low_10 = quantile(ZAGG_PHEARTAT,0.1),
                   Quar_1 = quantile(ZAGG_PHEARTAT,0.25),
                   median = median(ZAGG_PHEARTAT),
                   Quar_3 = quantile(ZAGG_PHEARTAT,0.75),
                   High_10 = quantile(ZAGG_PHEARTAT,0.9))
ZAGG_PHEARTAT_RBO = ZAGG_PHEARTAT_RBO[order(ZAGG_PHEARTAT_RBO$RBO_CM,decreasing = T),]
histogram(~ ZAGG_PHEARTAT|RBO_CM,data=pattern_0221,col="gray60",layout=c(1,4))

ZAGG_PHEARTAT_NFO <- pattern_0221 %>%
  group_by(NFO_CM)%>%
  dplyr::summarise(mean=mean(ZAGG_PHEARTAT),
                   Low_10 = quantile(ZAGG_PHEARTAT,0.1),
                   Quar_1 = quantile(ZAGG_PHEARTAT,0.25),
                   median = median(ZAGG_PHEARTAT),
                   Quar_3 = quantile(ZAGG_PHEARTAT,0.75),
                   High_10 = quantile(ZAGG_PHEARTAT,0.9))
ZAGG_PHEARTAT_NFO = ZAGG_PHEARTAT_NFO[order(ZAGG_PHEARTAT_NFO$NFO_CM,decreasing = T),]
histogram(~ ZAGG_PHEARTAT|NFO_CM,data=pattern_0221,col="gray60",layout=c(1,4))


# ANOVA TukeyHSD test
DECISION_STAGE_ANOVA = aov(pattern_0221$DECISION_STAGE~as.factor(pattern_0221$RBO_CM))
TukeyHSD(DECISION_STAGE_ANOVA)

RATE_INCR_ANOVA = aov(pattern_0221$RATE_INCR_UC~as.factor(pattern_0221$RBO_CM))
TukeyHSD(RATE_INCR_ANOVA)









