####################### See Tiv_Des_regimen_analysis.R #############################
rm(list=ls())
gc()

library(MASS)
library(blockcluster)

# Append first column
coclus_data = data.frame(PATIENT_ID=OTHER[,1],OTHER[,6:56])
class(coclus_data)

# Impute the missing value
coclus_data[is.na(coclus_data)]=0
colSums(is.na(coclus_data))

summary(coclus_data)
PATIENT_ID=coclus_data[,1]
coclus_data[coclus_data>1] <- 1
coclus_data <- data.frame(PATIENT_ID=PATIENT_ID,coclus_data[,2:ncol(coclus_data)])

# Convert to matrix
y <- as.matrix(coclus_data[,2:52])

# Look at data as matrix
dim(y)
coclus_model = cocluster(y, datatype = 'binary', nbcocluster=c(3,3))
# Summarize the output results
summary(coclus_model)
str(coclus_model)
coclus_model_data <- as.data.frame(coclus_model@coclusterdata)
coclus_model@colposteriorprob
# Plot the original and Co-clustered data 
par(mar=rep(2,4))
plot(coclus_model)
