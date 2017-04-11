# Some path analysis examples

source("path_analysis.R")

# Example with college football dataset
Data <- read.csv("sample_data_football_2015.csv", na.strings=c("", "NA"))
### First, just some data processing...
Data$date <- as.Date(as.character(Data$date))
Data$WIN <- as.integer(Data$margin > 0) # 1 if team 1, 0 if loss or tie
# Fill in N/A ranks with 50
Data2 <- Data
Data2[is.na(Data2$rank),"rank"] <- 50
Data2[is.na(Data2$opp_rank),"opp_rank"] <- 50
# Drop games beyond 12
Data2 <- Data2[Data2$game_number <= 12,]
### End data processing

# These are the variables to use for the clustering
cluster.vars <- c("WIN", "points", "opp_points", "rank")

# Convert the data from long to wide format - keep the variables for the team's
# weekly results
Paths <- long_to_wide(Data2, "team", "game_number", cluster.vars)

# Drop teams who played fewer than 12 games, so that the dataset is consistent
# width
Paths <- Paths[!is.na(Paths$WIN_12),]
# Then drop any N/As- this happens every once-in-a-blue-moon due to canceled
# games (which therefore don't have scores)
Paths <- na.omit(Paths)

# Loop over each variable and create smoothed versions
for (prefix in cluster.vars) {
  Paths <- create_smoothed_vars(Paths,
                                vars=which(startsWith(names(Paths), prefix)))
}

# Get clustering results
clust.out <- cluster_paths(Paths[2:ncol(Paths)], 5, scale=TRUE) # Exclude column 1 since it's the team name
# Create output paths dataset, with variables for cluster number, and whether
# the path is a *representative* path of its cluster
Paths.out <- Paths
Paths.out$cluster <- clust.out$cluster
Paths.out$Is_Representative <- clust.out$is.rep

# Let's take a look at the cluster representatives:
Paths.out[Paths.out$Is_Representative,]

# Let's summarize each variable by cluster, longitudinally:
aggregate(Paths.out[2:(ncol(Paths.out)-2)], by=Paths.out['cluster'], FUN=mean)
# And summarize each variable by cluster, ignoring time effects:
Data.out <- merge(Data2, Paths.out[c('team', 'cluster', 'Is_Representative')])
aggregate(Data.out[sapply(Data.out, is.numeric)], by=Data.out['cluster'],
          FUN=function(x) mean(x, na.rm=TRUE))

# Save results
write.csv(Paths.out, "sample_paths_dataset_clustered_wide.csv", row.names=FALSE)
write.csv(Data.out, "sample_paths_dataset_clustered_long.csv", row.names=FALSE)



