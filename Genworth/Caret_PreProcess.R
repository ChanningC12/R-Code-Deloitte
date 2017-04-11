data(crimtab)
head(crimtab)
class(crimtab)
help(crimtab)
dim(crimtab)
str(crimtab)

sum(crimtab)
colnames(crimtab)

apply(crimtab,2,var)
pca=prcomp(crimtab)
summary(pca) # PC1 explained 75.22% of the variance
plot(pca)
pca$rotation = -pca$rotation
pca$x = -pca$x
biplot(pca,scale=0)

str(pca)

##############################
library(AppliedPredictiveModeling)
data("schedulingData")
str(schedulingData)

pp_hpc = preProcess(schedulingData[,-8],method=c("center","scale","YeoJohnson"))
pp_hpc
transformed = predict(pp_hpc,newdata=schedulingData[,-8])
summary(transformed)
pp_no_nzv = preProcess(schedulingData[,-8],
                       method=c("center","scale","YeoJohnson","nzv"))
pp_no_nzv
predict(pp_no_nzv,newdata=schedulingData[1:6,-8])









