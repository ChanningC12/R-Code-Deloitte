library(reshape)

ads_360 = read.csv("CUSTOMER36010ads_360_mnth_filtered.csv")
names(ads_360)

ads_360 = ads_360[,c(1:4,69:70)]

# the code is not working
ads_360_reshape = reshape(ads_360, idvar = c("pfz_cust_id","brand"), timevar = c("year","month"),
                          direction = "wide")

ads_360_new = ads_360
ads_360_new$cust_brand = paste(ads_360_new$pfz_cust_id,ads_360_new$brand,sep="_")
ads_360_new$yr_mnth = paste(ads_360_new$year,ads_360_new$month,sep="_")
ads_360_new = ads_360_new[,c(7:8,5:6)]

ads_360_reshape = reshape(ads_360_new, idvar = c("cust_brand"), timevar = c("yr_mnth"),
                          direction = "wide")

####### Lag ##########
# Create lag variable on BrandTRx dol
ads_360$month = ts(ads_360$month)
ads_360 = ads_360[order(ads_360$pfz_cust_id,ads_360$brand,ads_360$year),]
ads_360_Pre = DataCombine::slide(ads_360,Var="brand_trx_dol",GroupVar = c("pfz_cust_id","brand","year"), slideBy = -1)

library(data.table)
ads_360_lag = ads_360
ads_360_lag = as.data.table(ads_360_lag)
ads_360_lag = ads_360_lag[, lag.value:=c(NA, brand_trx_dol[-.N]), by=c("pfz_cust_id","brand","year")]



