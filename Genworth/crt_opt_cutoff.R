
setwd("C:\\Users\\pribanerjee\\Documents\\ARA\\AAM\\LIFE PM\\GENWORTH\\MODELING\\SCORE")

### Read in scored data
nfo_val_data <- read.csv("nfo_val.csv",header=TRUE)
rbo_val_data <- read.csv("rbo_val.csv",header=TRUE)

library(ROCR)

## Create cuts for NFO
pred_nfo <- prediction(nfo_val_data$Prob_NFO, nfo_val_data$Actual)
perf_nfo <- performance(pred_nfo,"tpr","fpr")

cutoffs_nfo <- data.frame(cut=perf_nfo@alpha.values[[1]], fpr=perf_nfo@x.values[[1]], 
                      tpr_sens=perf_nfo@y.values[[1]], tnr_specf=1-perf_nfo@x.values[[1]],
                      accuracy=(perf_nfo@y.values[[1]]+1-perf_nfo@x.values[[1]]), ## Accuracy calculation is wrong
                      youdens_j = perf_nfo@y.values[[1]]+(1-perf_nfo@x.values[[1]])-1)


cutoffs_sort_nfo_j <- cutoffs_nfo[order(-cutoffs_nfo[,6]),]

## Create cuts for RBO
pred_rbo <- prediction(rbo_val_data$Prob_RBO, rbo_val_data$Actual)
perf_rbo <- performance(pred_rbo,"tpr","fpr")

cutoffs_rbo <- data.frame(cut=perf_rbo@alpha.values[[1]], fpr=perf_rbo@x.values[[1]], 
                          tpr_sens=perf_rbo@y.values[[1]], tnr_specf=1-perf_rbo@x.values[[1]],
                          accuracy=(perf_rbo@y.values[[1]]+1-perf_rbo@x.values[[1]]), ## Accuracy calculation is wrong
                          youdens_j = perf_rbo@y.values[[1]]+(1-perf_rbo@x.values[[1]])-1)

cutoffs_sort_rbo_j <- cutoffs_rbo[order(-cutoffs_rbo[,6]),]

################################ Genworth Models ########################################

### Read in scored data
nfo_val_gn <- read.csv("nfo_scored_1.csv",header=TRUE)
rbo_val_gn <- read.csv("rbo_scored_1.csv",header=TRUE)


## Create cuts for NFO
pred_nfo_gn <- prediction(nfo_val_gn$y_hat, nfo_val_gn$NFO)
perf_nfo_gn <- performance(pred_nfo_gn,"tpr","fpr")

cutoffs_nfo_gn <- data.frame(cut=perf_nfo_gn@alpha.values[[1]], fpr=perf_nfo_gn@x.values[[1]], 
                             tpr_sens=perf_nfo_gn@y.values[[1]], tnr_specf=1-perf_nfo_gn@x.values[[1]],
                             accuracy=(perf_nfo_gn@y.values[[1]]+1-perf_nfo_gn@x.values[[1]]),## Accuracy calculation is wrong
                             youdens_j = perf_nfo_gn@y.values[[1]]+(1-perf_nfo_gn@x.values[[1]])-1)

cutoffs_sort_nfo_gn_j <- cutoffs_nfo_gn[order(-cutoffs_nfo_gn[,6]),]

## Create cuts for RBO
pred_rbo_gn <- prediction(rbo_val_gn$y_hat, rbo_val_gn$RBO)
perf_rbo_gn <- performance(pred_rbo_gn,"tpr","fpr")

cutoffs_rbo_gn <- data.frame(cut=perf_rbo_gn@alpha.values[[1]], fpr=perf_rbo_gn@x.values[[1]], 
                             tpr_sens=perf_rbo_gn@y.values[[1]], tnr_specf=1-perf_rbo_gn@x.values[[1]],
                             accuracy=(perf_rbo_gn@y.values[[1]]+1-perf_rbo_gn@x.values[[1]]),## Accuracy calculation is wrong
                             youdens_j = perf_rbo_gn@y.values[[1]]+(1-perf_rbo_gn@x.values[[1]])-1)

cutoffs_sort_rbo_gn_j <- cutoffs_rbo_gn[order(-cutoffs_rbo_gn[,6]),]
