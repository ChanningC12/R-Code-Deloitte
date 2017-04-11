#Instructions for setting up http://r4ds.had.co.nz/introduction.html#prerequisites Section 1.4.1 
#Using the Styleguide from Google : https://google.github.io/styleguide/Rguide.xml

# Import a library
# Libraries contain tons of useful functions written by others
# Typically at the cutting edge of statistical 
#install.packages("plyr")
library(plyr)

rm(list = ls())		# clears the workspace

setwd("C:\\Users\\avesper\\Desktop\\R Course Material\\Data\\")
meps <- read.csv("MEPS.Diabetes.SOA.csv") #Also look at read.table

#Different ways of "exploring the data"
summary(meps)
str(meps) #This is where you make sure you that all variables have the right "type"

dim(meps) #Dimensions of the dataframe
nrow(meps)
ncol(meps)

# Explore a few rows of data
head(meps)
tail(meps)

# Can access the variables in the meps data using the $ or by column name or by column number
head(meps$HHID)
head(meps[,c('HHID')])
head(meps[,1])
# Can also slice by row numbers
meps[1:6,1]

length(meps$HHID) #Used mainly for vectors
mean(meps$EXPEND)

#General help (Question mark followed by function name)
?summary

#Looking at the names of all the variables in the dataframe
names(meps)

#Use subset when you want to get a subset of the data 
# Consider the subset of meps for age > 50 and the AGE and INCOME columns
meps.subset <- subset(meps, AGE > 50, select = c("AGE", "INCOME"))
names(meps.subset)

#Another way to subset the data
meps.subset.2 <- meps[meps$AGE > 50, c("AGE", "INCOME")]
names(meps.subset.2)

#Checking the results of the two subsets? See if the number of rows is the same
nrow(meps.subset) == nrow(meps.subset.2)

summary(meps$AGE)
summary(meps.subset$AGE)

# Sorting a vector of data
sort(meps$AGE)
# order
# Ordering a data set
meps <- meps[order(meps$AGE),]
head(meps)

# Merging data sets
# Consider two subsets of the meps data
meps.employed <- subset(meps,EMPLOYED==1,c('PERSONID','INCOME','EMPLOYED'))
meps.unemployed <- subset(meps,EMPLOYED==0,c('PERSONID','INCOME','EMPLOYED'))
# Can combine the data sets by stacking using rbind
meps.all <- rbind(meps.employed,meps.unemployed)
dim(meps.employed)
dim(meps.unemployed)
dim(meps.all)
# Can also perform a "join"
?merge
# Inner join
meps.merge <- merge(meps,meps.employed[,c('PERSONID','INCOME')],by='PERSONID')
head(meps.merge)
# Left join
meps.merge.all <- merge(meps,meps.employed[,c('PERSONID','INCOME')],by='PERSONID',all.x=T)
head(meps.merge.all)

# Renaming a column in the data set
names(meps)
# Rename the FEMALE column to GENDER
colnames(meps)[colnames(meps) == 'FEMALE'] <- 'GENDER'
names(meps)
# Easier way using the rename function in the plyr package
# Rename the GENDER column back to FEMALE
meps <- rename(meps,c('GENDER'='FEMALE')) # Using plyr
names(meps)


######As an exercise let's simulate how we can make a subset of data that has no missing values.

meps.no.missing <- meps  #We leave the original dataset unmodified so as to have independent analysis based on the data.
summary(meps.no.missing$SMOKER)

#All possible values of SMOKER flag
unique(meps.no.missing$SMOKER)
count(meps.no.missing$SMOKER)  #From plyr package

#99 is actually recoding of missing values. R represents missing values by NA
head(meps.no.missing$SMOKER)

#Replacing 99 with NA
meps.no.missing[meps.no.missing$SMOKER == 99, c('SMOKER')] <- NA
count(meps.no.missing$SMOKER)

#Subset to remove rows with missing values for SMOKER 
# is.na is a function that returns either TRUE or FALSE if the value is NA
meps.no.missing = subset(meps.no.missing, subset = !is.na(SMOKER)) 
count(meps.no.missing$SMOKER)


### Recoding variables - Sometimes you really want a yes or a no instead of 0 or 1.  

meps.recoded <- meps
# Convert to a factor variable
# This treats CHOLEST as categorical as opposed to numerical
meps.recoded$CHOLEST <- as.factor(meps.recoded$CHOLEST)
levels(meps.recoded$CHOLEST)

#mapvalues works only on factors
meps.recoded$CHOLEST <- mapvalues(meps.recoded$CHOLEST, c("0","1"), c("No","Yes"))

#Checking the results
head(meps$CHOLEST)
head(meps.recoded$CHOLEST)

#More Points to cover
#Merging data (http://www.statmethods.net/management/merging.html)
#plyr (http://r4ds.had.co.nz/transform.html#dplyr-verbs) 

#Other packages to look at
#data.table
#reshape2 