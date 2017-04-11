getwd()
setwd("../Desktop/")
uni_heat = read.csv("Heatmap.csv")
str(uni_heat)

uni_heat$NFO_IND = ifelse(uni_heat$RESPONSE=="NFO",1,0)
uni_heat$RBO_IND = ifelse(uni_heat$RESPONSE=="RBO",1,0)

table(uni_heat$NFO_IND)
table(uni_heat$RBO_IND)

uni_heat$POLICY_AGE_GROUP = ifelse(uni_heat$POLICY_AGE_2<=10,"<10",
                                   ifelse(uni_heat$POLICY_AGE_2<=11,"11",
                                    ifelse(uni_heat$POLICY_AGE_2<=12,"12",
                                           ifelse(uni_heat$POLICY_AGE_2<=13,"13",">13"))))
uni_heat$POLICYHOLDER_AGE_GROUP = ifelse(uni_heat$POLICYHOLDER_AGE<=67,"<=67",
                                   ifelse(uni_heat$POLICYHOLDER_AGE<=72,"67<-72",
                                          ifelse(uni_heat$POLICYHOLDER_AGE<=76,"72<-76",">76")))

uni_heat$POLICY_AGE_GROUP = factor(uni_heat$POLICY_AGE_GROUP,levels=c("<10","11","12","13",">13"))
uni_heat$POLICYHOLDER_AGE_GROUP = factor(uni_heat$POLICYHOLDER_AGE_GROUP,levels=c("<=67","67<-72","72<-76",">76"))

table(uni_heat$POLICY_AGE_GROUP)
table(uni_heat$POLICYHOLDER_AGE_GROUP)

library(data.table)
uni_heat_dt = as.data.table(uni_heat)
uni_heat_agg = uni_heat_dt[,.(NFO = mean(NFO_IND), 
                              RBO = mean(RBO_IND),
                              count = .N),by=.(POLICY_AGE_GROUP,POLICYHOLDER_AGE_GROUP)]
uni_heat_agg = uni_heat_agg[complete.cases(uni_heat_agg),]

# heatmap
library(ggplot2)
gg_nfo = ggplot(uni_heat_agg,aes(x=POLICY_AGE_GROUP,y=POLICYHOLDER_AGE_GROUP,fill=NFO))
gg_nfo = gg_nfo + geom_tile(color="white",size=0.1) + scale_fill_gradient(low="yellow",high="red")
gg_nfo = gg_nfo + geom_text(aes(label=paste(100*round(NFO,4),"%",sep="")))
gg_nfo = gg_nfo + ggtitle("NFO Take Rate by Attained Age and Policy Duration")
gg_nfo

gg_rbo = ggplot(uni_heat_agg,aes(x=POLICY_AGE_GROUP,y=POLICYHOLDER_AGE_GROUP,fill=RBO))
gg_rbo = gg_rbo + geom_tile(color="white",size=0.1) + scale_fill_gradient(low="green1",high="green4")
gg_rbo = gg_rbo + geom_text(aes(label=paste(100*round(RBO,4),"%",sep="")))
gg_rbo = gg_rbo + ggtitle("RBO Take Rate by Attained Age and Policy Duration")
gg_rbo

# 12-11 SOM Example
## Load the package - kohonen
require(kohonen)
sapply(uni_heat,function(x) sum(is.na(x)))
uni_heat_matrix = subset(uni_heat,select=c("ISSUE_AGE","POLICY_AGE_2","POLICYHOLDER_AGE"))
uni_heat_matrix = uni_heat_matrix[complete.cases(uni_heat_matrix),]
# transfer to matrix format
uni_heat_matrix = as.matrix(scale(uni_heat_matrix))
# set up grid for som
som_grid = somgrid(xdim=4,ydim=4,topo = "rectangular")
# build som model
som_model = som(uni_heat_matrix,grid = som_grid)
som_model
summary(som_model)
str(som_model)
plot(som_model,type="changes")
# count of each node
plot(som_model,type="count")
# som heatmap
uni_heat_nona = uni_heat[complete.cases(uni_heat),]
nfo_unscaled = aggregate(as.numeric(uni_heat_nona[,5]),
                         by=list(som_model$unit.classif),FUN=mean,
                         simplify=T)[,2]
plot(som_model,type="property",
     property=nfo_unscaled,main=names(uni_heat_nona[5]))

rbo_unscaled = aggregate(as.numeric(uni_heat_nona[,6]),
                         by=list(som_model$unit.classif),FUN=mean,
                         simplify=T)[,2]
plot(som_model,type="property",
     property=rbo_unscaled,main=names(uni_heat_nona[6]))

# merge back the unit.classif
unit_heat_nona_unit = data.frame(uni_heat_nona,cluster = som_model$unit.classif)
unit_heat_nona_unit_dt = as.data.table(unit_heat_nona_unit)
result = unit_heat_nona_unit_dt[,.(count = .N,
                                   avg.issue_age = round(mean(ISSUE_AGE),2),
                          avg.policy_age = round(mean(POLICY_AGE_2),2),
                          avg.policyholder_age = round(mean(POLICYHOLDER_AGE),2),
                          NFO = round(mean(NFO_IND),2),
                          RBO = round(mean(RBO_IND),2)),
                       by = cluster]
str(result)
result = result[order(cluster),]
result
text(som_model$unit.classif)

