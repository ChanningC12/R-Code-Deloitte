# set up directory
getwd()
setwd("D:/Channing/")

# read in pre_cap_2_2 dataset
pre_cap_2 = read.csv("allvars_precap.csv",na.strings = c("","NA"))
str(pre_cap_2)


# Impute missing values: numerical to median, character to mode
pre_cap_2_dup = pre_cap_2

for (x in 1:ncol(pre_cap_2_dup)) {
  if(class(pre_cap_2_dup[,x]) %in% c("numeric","integer")) {
    pre_cap_2_dup[is.na(pre_cap_2_dup[,x]),x] = median(pre_cap_2_dup[,x],na.rm=T)
  }
  else if(class(pre_cap_2_dup[,x]) == "factor") {
    pre_cap_2_dup[is.na(pre_cap_2_dup[,x]),x] = Mode(pre_cap_2_dup[,x],na.rm=T)
  }
}


# generate a list of median / mode
pre_cap_2_num = pre_cap_2_dup[,sapply(pre_cap_2_dup,class) %in% c("numeric","integer")]
pre_cap_2_char = pre_cap_2_dup[,sapply(pre_cap_2_dup,class) %in% c("factor","logical")]

pre_cap_2_num_median = sapply(pre_cap_2_num,median,na.rm=T)
pre_cap_2_char_Mode = sapply(pre_cap_2_char,Mode,na.rm=T)

pre_cap_2_num_median_ls = data.frame(pre_cap_2_num_median)
pre_cap_2_char_Mode_ls = data.frame(t(pre_cap_2_char_Mode))
pre_cap_2_char_Mode_ls = data.frame(t(pre_cap_2_char_Mode_ls))

pre_cap_2_char_Mode_ls$t.pre_cap_2_char_Mode_ls. = as.character(pre_cap_2_char_Mode_ls$t.pre_cap_2_char_Mode_ls.)

str(pre_cap_2_num_median_ls)
str(pre_cap_2_char_Mode_ls)

# write.csv
write.csv(pre_cap_2_num_median_ls,"Missing_Impute_NUM.csv")
write.csv(pre_cap_2_char_Mode_ls,"Missing_Impute_CHAR.csv")







