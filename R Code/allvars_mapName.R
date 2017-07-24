rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)
library(ggplot2)
library(rattle)

allvars <- fread("allvars_0619.csv")
colSums(is.na(allvars))
str(allvars)

region_map <- fread("region_map.csv")
allvars <- merge(allvars,region_map,by="Region",all.x=T)

fwrite(allvars,"allvars_0619_map.csv")
