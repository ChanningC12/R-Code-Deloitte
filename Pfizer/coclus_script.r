library(MASS)
library(blockcluster)

# Read in the data
coclus_data<-read.csv("../Desktop/coclus_data_cleansed.csv",na.strings=c("NA","NaN",""))
str(coclus_data)

# Convert all columns to numeric
coclus_data_sub <- lapply(coclus_data[,2:ncol(coclus_data)], function(x) as.numeric(as.character(x)))
coclus_data_sub = as.data.frame(coclus_data_sub)
str(coclus_data_sub)

# Append first column
coclus_data = data.frame(Resource=coclus_data[,1],coclus_data_sub)
class(coclus_data)
str(coclus_data)

# Impute the missing value
coclus_data[is.na(coclus_data)]=0
colSums(is.na(coclus_data))

# Convert to matrix
y <- as.matrix(coclus_data)

# Look at data as matrix
dim(y)

coclus_model = cocluster(y, datatype = 'continuous', nbcocluster=c(2,2))
