library(blockcluster)
# Simple example with simulated binary data
#load data
data(binarydata)
#usage of cocluster function in its most simplest form
out<-cocluster(binarydata,datatype="binary",nbcocluster=c(2,3))
#Summarize the output results
summary(out)
#Plot the original and Co-clustered data 
plot(out)


# A little advanced example with simulated gaussian data
data(gaussiandata)
#set strategy , see documentation for "cocluststrategy" function for more details.
newstrategy<-coclusterStrategy(nbxem=5,nbtry=2)
# calling cocluster function with newstrategy and default model
out<-cocluster(gaussiandata,datatype="continuous",nbcocluster=c(2,3),strategy=newstrategy)
#Summarize the output results
summary(out)
#Plot the original and Co-clustered data 
plot(out)



##### Clustering mixed data types in R #####
# https://www.r-bloggers.com/clustering-mixed-data-types-in-r/

library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

college_clean <- College %>%
  mutate(name = row.names(.),
         accept_rate = Accept/Apps,
         isElite = cut(Top10perc,
                       breaks = c(0, 50, 100),
                       labels = c("Not Elite", "Elite"),
                       include.lowest = TRUE)) %>%
  mutate(isElite = factor(isElite)) %>%
  select(name, accept_rate, Outstate, Enroll,
         Grad.Rate, Private, isElite)

glimpse(college_clean)

# 1. Calculating distance
# Euclidean distance is only valid for continuous variables
# Gower distance
# quantitative (interval): range-normalized Manhattan distance
# ordinal: variable is first ranked, then Manhattan distance is used with a special adjustment for ties
# nominal: variables of k categories are first converted into k binary columns and then the Dice coefficient is used

# Remove college name before clustering

gower_dist <- daisy(college_clean[, -1],
                    metric = "gower",
                    type = list(logratio = 3))
# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"

summary(gower_dist)

# display the most similar and dissimilar
gower_mat <- as.matrix(gower_dist)
dim(gower_mat)

# Output most similar pair
college_clean[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair
college_clean[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]


# 2. Choosing a clustering algorithm
# partitioning around medoids (PAM)
# If you know the k-means algorithm, this might look very familiar. In fact, both approaches are identical, except k-means has cluster centers defined by Euclidean distance (i.e., centroids), while cluster centers for PAM are restricted to be the observations themselves (i.e., medoids).

# 3. Selecting the number of clusters
# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
pam_results <- college_clean %>%
  dplyr::select(-name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

college_clean[pam_fit$medoids, ]

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = college_clean$name)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

tsne_data %>%
  filter(X > 15 & X < 25,
         Y > -15 & Y < -10) %>%
  left_join(college_clean, by = "name") %>%
  collect %>%
  .[["name"]]


