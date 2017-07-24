rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)
library(ggplot2)

nbrx <- read.csv("NBRx_Share_brand.csv")

# Calculate market share
nbrx$triumeq_NBRx_post_12_share <- nbrx$triumeq_NBRx_post_12/nbrx$Mkt_NBRx_post_12
nbrx$triumeq_NBRx_post_6_share <- nbrx$triumeq_NBRx_post_6/nbrx$Mkt_NBRx_post_6
nbrx$triumeq_NBRx_pre_6_share <- nbrx$triumeq_NBRx_pre_6/nbrx$Mkt_NBRx_pre_6
nbrx$triumeq_NBRx_pre_12_share <- nbrx$triumeq_NBRx_pre_12/nbrx$Mkt_NBRx_pre_12

nbrx$tivicay_NBRx_post_12_share <- nbrx$tivicay_NBRx_post_12/nbrx$Mkt_NBRx_post_12
nbrx$tivicay_NBRx_post_6_share <- nbrx$tivicay_NBRx_post_6/nbrx$Mkt_NBRx_post_6
nbrx$tivicay_NBRx_pre_6_share <- nbrx$tivicay_NBRx_pre_6/nbrx$Mkt_NBRx_pre_6
nbrx$tivicay_NBRx_pre_12_share <- nbrx$tivicay_NBRx_pre_12/nbrx$Mkt_NBRx_pre_12

nbrx$g_s_NBRx_post_12_share <- nbrx$g_s_NBRx_post_12/nbrx$Mkt_NBRx_post_12
nbrx$g_s_NBRx_post_6_share <- nbrx$g_s_NBRx_post_6/nbrx$Mkt_NBRx_post_6
nbrx$g_s_NBRx_pre_6_share <- nbrx$g_s_NBRx_pre_6/nbrx$Mkt_NBRx_pre_6
nbrx$g_s_NBRx_pre_12_share <- nbrx$g_s_NBRx_pre_12/nbrx$Mkt_NBRx_pre_12

nbrx$o_c_NBRx_post_12_share <- nbrx$o_c_NBRx_post_12/nbrx$Mkt_NBRx_post_12
nbrx$o_c_NBRx_post_6_share <- nbrx$o_c_NBRx_post_6/nbrx$Mkt_NBRx_post_6
nbrx$o_c_NBRx_pre_6_share <- nbrx$o_c_NBRx_pre_6/nbrx$Mkt_NBRx_pre_6
nbrx$o_c_NBRx_pre_12_share <- nbrx$o_c_NBRx_pre_12/nbrx$Mkt_NBRx_pre_12

nbrx$truvada_NBRx_post_12_share <- nbrx$truvada_NBRx_post_12/nbrx$Mkt_NBRx_post_12
nbrx$truvada_NBRx_post_6_share <- nbrx$truvada_NBRx_post_6/nbrx$Mkt_NBRx_post_6
nbrx$truvada_NBRx_pre_6_share <- nbrx$truvada_NBRx_pre_6/nbrx$Mkt_NBRx_pre_6
nbrx$truvada_NBRx_pre_12_share <- nbrx$truvada_NBRx_pre_12/nbrx$Mkt_NBRx_pre_12

nbrx$epzicom_NBRx_post_12_share <- nbrx$epzicom_NBRx_post_12/nbrx$Mkt_NBRx_post_12
nbrx$epzicom_NBRx_post_6_share <- nbrx$epzicom_NBRx_post_6/nbrx$Mkt_NBRx_post_6
nbrx$epzicom_NBRx_pre_6_share <- nbrx$epzicom_NBRx_pre_6/nbrx$Mkt_NBRx_pre_6
nbrx$epzicom_NBRx_pre_12_share <- nbrx$epzicom_NBRx_pre_12/nbrx$Mkt_NBRx_pre_12

nbrx$atripla_NBRx_post_12_share <- nbrx$atripla_NBRx_post_12/nbrx$Mkt_NBRx_post_12
nbrx$atripla_NBRx_post_6_share <- nbrx$atripla_NBRx_post_6/nbrx$Mkt_NBRx_post_6
nbrx$atripla_NBRx_pre_6_share <- nbrx$atripla_NBRx_pre_6/nbrx$Mkt_NBRx_pre_6
nbrx$atripla_NBRx_pre_12_share <- nbrx$atripla_NBRx_pre_12/nbrx$Mkt_NBRx_pre_12

nbrx$Isentress_NBRx_post_12_share <- nbrx$isentress_NBRx_post_12/nbrx$Mkt_NBRx_post_12
nbrx$isentress_NBRx_post_6_share <- nbrx$isentress_NBRx_post_6/nbrx$Mkt_NBRx_post_6
nbrx$isentress_NBRx_pre_6_share <- nbrx$isentress_NBRx_pre_6/nbrx$Mkt_NBRx_pre_6
nbrx$isentress_NBRx_pre_12_share <- nbrx$isentress_NBRx_pre_12/nbrx$Mkt_NBRx_pre_12

fwrite(nbrx,"../../Summary/nbrx_share_0626.csv",row.names = F)






