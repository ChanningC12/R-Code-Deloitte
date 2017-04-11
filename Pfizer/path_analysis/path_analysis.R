# path_analysis.R
#
# Library of functions for pathway analytics

require(cluster)
require(TDA)


EPS <- 1e-5


# This is the main function for grouping patient pathways into clusters.
#
# Args:
#  Paths: Dataframe of patient pathways. Each row must be one patient, and all
#    columns must be variables defining the patients' pathways which will be
#    used as variables for the clustering algorithm. These columns may represent
#    any mix of relevant variables such as patient states in a given time
#    period, moving averaged states in a time period, aggregated synthetic
#    variables, etc. Factor and character variables are acceptable too; they
#    will be converted into 0/1 indicators.
#  num.clusters: Positive integer, number of clusters to group patients into
#  num.reps: Number of "cluster representatives" to identify within each
#    cluster. Default is one, which selects the one patient pathway which is
#    considered to be the most representative or typical within each cluter.
#  scale: If true (default), variables are scaled by the R scale() function,
#    so that variables with greater magnitudes will not necessarily have a
#    greater importance. If false, the input variables are not scaled, so
#    variables with greater absolute differences will tend to be more important
#    for the clustering.
#    IMPORTANT NOTE: If you choose scale=FALSE, be careful if including
#    categorical (factor) variables. Factors are converted into 0/1 indicators,
#    so if the range of your numeric variables is orders of magnitude greater
#    than 1, then the categorical variables will be largely drowned out. You
#    can fix this by manually scaling all numeric variables by some constant
#    factor (e.g. dividing all numeric variables by the standard deviation of
#    the most important numeric variable).
#  do.PC: If true (default), variables are converted into principal components
#    before clustering. This can potentially lead to more stable results. Note
#    that do.PC will not scale the variables if scale=FALSE is specified.
#  method: Character string, clustering method to use. Options:
#    pam (default): Partitioning Around Medoids
#    kmeans: k-means
#    hclust: hierarchical clustering
#    agnes: agglomerative hierarchical clustering
#    clusterTree: Density Based Clustering as implemented by the clusterTree
#      function from package TDA
#  kNN: For method="clusterTree", this is the number of nearest neighbors to use
#    for the k-nearest neighbors linkage. Higher values (more neighbors) may
#    tend to produce more ... TODO finish this comment & do some research
#  ...: Additional arguments passed to clustering function
#
# Returns a list with elements:
#  cluster: Vector of integers indicating which cluster each path belongs to.
#    cluster[i] is the cluster membership of the row Paths[i,].
#  reps: List of vectors of integers indicating the numbers of the patients
#    who are the most central or representative members of each cluster. To
#    be specific, reps[[j]] is a vector of integers (i1, i2, ...) such that
#    the patient pathway(s) represented by Paths[i1,], Paths[i2,], etc. are
#    the representative members of Cluster j.
#  is.rep: Logical vector such that is.rep[i] is true if Paths[i,] is a
#    representative for one of the clusters; false otherwise
#  model: Model object from clustering function
cluster_paths <- function(Paths, num.clusters, num.reps=1,
                          scale=TRUE, do.PC=TRUE, method="pam", kNN, ...) {
  # Check args
  if (!is.data.frame(Paths) || nrow(Paths) == 0 || ncol(Paths) == 0) {
    stop("Paths must be a non-empty dataframe")
  } 

  # Look for any factors/characters with only one level, to avoid errors in
  # model.matrix and because these can't really help with clustering anyway
  for (j in 1:ncol(Paths)) {
    if ((is.character(Paths[[j]]) || is.factor(Paths[[j]]))
        && length(unique(Paths[[j]])) == 1) {
      stop(paste("Variable", names(Paths[j]), "has only one level - cannot",
                 "use it for clustering"))
    }
  }
  na.action.sav <- options("na.action")
  options(na.action="na.pass")
  tryCatch({
    Paths.mat <- model.matrix(~ ., Paths)
  }, finally={
    options(na.action=na.action.sav)
  })
  # Drop any constant columns (including intercept) - these are useless for
  # clustering and will likely cause errors
  Paths.mat <- Paths.mat[,apply(Paths.mat, 2, function(v) max(v)-min(v) > EPS)]

  # Scale columns if requested
  if (scale) {
    Paths.mat <- scale(Paths.mat, center=FALSE)
  }

  # Apply principal components if requested
  if (do.PC) {
    Paths.mat <- prcomp(Paths.mat)$x
  }

  # Perform clustering
  if (method == "pam") {
    model <- pam(Paths.mat, num.clusters, ...)
    cluster <- model$cluster
  } else if (method == "kmeans") {
    model <- kmeans(Paths.mat, num.clusters, ...)
    cluster <- model$cluster
  } else if (method == "hclust") {
    model <- hclust(dist(Paths.mat), ...)
    cluster <- cutree(model, num.clusters)
  } else if (method == "agnes") {
    model <- agnes(Paths.mat, ...)
    cluster <- cutree(model, num.clusters)
  } else if (method == "clusterTree") {
    model <- clusterTree(Paths.mat, k=kNN, ...)
    cluster <- cut_clusterTree(model, num.clusters)
  } else {
    stop(paste("Unrecognized clustering method:", method))
  }

  # Get cluster representatives
  reps <- list()
  for (clust in unique(na.omit(cluster))) {
    members <- which(cluster == clust)
    id.med <- pam(Paths.mat[members,], min(num.reps, length(members)-1))$id.med
    reps[[clust]] <- as.integer(members[id.med])
  }

  is.rep <- 1:nrow(Paths.mat) %in% unlist(reps)

  return(list(cluster=cluster, reps=reps, is.rep=is.rep, model=model))
}




# Creates smoothed, weighted moving-average versions of time-based variables.
# This is useful as a pre-processing step before running clustering (i.e.
# before calling the cluster_paths function); it tends to create better
# clustering results if paths are smoothed over time so that similar data in
# adjacent or nearby time periods can be recognized as a sign of similarity by
# the algorithm.
#
# Args:
#  Paths: Dataframe of patient pathways. Each row must be one patient
#  vars: Variables in the Paths dataset which represent the various states
#    and/or events of each patient over time, in order. For example, if the
#    paths represent a patient's "state" over various months, then vars could
#    be c("state_month1", "state_month2", "state_month3", etc.)
#    By default, the function uses all columns from Paths *except* the first
#    column (since the first column will generally be a patient ID).
#    Notes:
#    1. The order of the column names is important - that's how the function
#       figures out the sequential order of the variables. E.g., it
#       needs to be vars=c("state_month1", "state_month2", "state_month3", ...),
#       *NOT* vars=c("state_month3", "state_month1", "state_month2", ...)!
#    2. vars can be provided either as a list of character names, or integer
#       indexes - e.g. if the column names in paths are "x", "y", and "z", then
#       vars=c("x", "z") or vars=c(1, 3) are both OK and have the same result.
#  weights: Numeric weights defining how the weighted moving averages are
#    calculated. When calculating the smoothed value of the variable in a given##    time period t, weights[1] is the weight to give to the variable in time t,
#    weights[2] is the weight given to time t+1, weight[3] is for time t+2, etc.
#    For example, suppose that vars=2:10, and weights=c(0.7, 0.2, 0.1) (which is
#    the default). Then, the value in the first row and column of the output
#    dataset (the first column of course corresponding to the first time period,
#    Column 2 in this example) is:
#      Out[1,1] = 0.7*Paths[1,2] + 0.2*Paths[1,3] + 0.1*Paths[1,4]
#    Note that inside the function, weights are automatically normalized to sum 
#    to 1.
#  suffix: Suffix to be added to the newly created smoothed variables. Default
#    is ".smooth".
#  return.whole.dataset: If true (default), the entire original dataset "Paths"
#    is returned, with the new smoothed variables appended to the righthand side
#    as new columns. If false, only the new columns are returned (with the rows
#    in the same order as the input dataset Paths, of course).
#
# Returns: Dataframe with one column for each column in "vars", with smoothed
#  values of each of the variables in Paths[vars].
#  (If return.whole.dataset=TRUE, then the return value will also contain all of
#  the original columns of Paths as well.)
create_smoothed_vars <- function(Paths, vars=2:ncol(Paths),
                                 weights=c(.7, .2, .1),
                                 suffix=".smooth", return.whole.dataset=TRUE) {
  # Check args
  if (!is.data.frame(Paths) || nrow(Paths) < 1 || ncol(Paths) < 2) {
    stop("Paths must be a non-empty dataframe with at least 2 columns")
  }
  if (!all(sapply(Paths[vars], is.numeric))) {
    stop("All variables in Paths[vars] must be numeric")
  }
  if (!is.numeric(weights) || length(weights) < 1
      || any(is.na(weights)) || any(weights < 0.) || all(weights == 0.)) {
    stop(paste("weights must be a nonnegative vector with no missing values",
               "and at least one nonzero entry"))
  }

  weights <- weights / sum(weights)

  # Select relevant variables, in order, so column 1 is the first time period
  Smoothed <- Paths[vars]
  # Create a temporary dataset with a buffer of zero-value columns to the
  # righthand side to avoid index-out-of-range errors
  Tmp <- cbind(Smoothed, matrix(0., nrow(Smoothed), length(weights) - 1))
  Smoothed <- 0. * Smoothed # Clear out to zero
  for (j in 1:length(weights)) {
    Smoothed <- Smoothed + weights[j] * Tmp[j:(ncol(Smoothed)+j-1)]
  }

  # Rename the new smoothed variables (add the suffix to the end)
  names(Smoothed) <- make.names(c(names(Paths), paste0(names(Smoothed),suffix)),
                      unique=TRUE)[(ncol(Paths)+1):(ncol(Paths)+ncol(Smoothed))]

  if (return.whole.dataset) {
    Smoothed <- cbind(Paths, Smoothed)
  }
  return(Smoothed)
}


# Wrapper for reshape() which reshapes path data from "long" format (with one
# row per patient-time) to "wide" format (one row per patient, one column per
# time per variable).
#
# Args:
#  Long.data: Dataframe with one row per patient-time - it must contain an
#    ID column identifying the patient, a time column identifying the time, and
#    one or more columns containing the values of variables of interest for each
#    patient at each point in time.
#  idvar: Name of the column containing the patient identifier
#  timevar: Name of the column containing the time (month, date, etc.)
#  vars: Names of the column or columns containing the variables of interest,
#    which are to be converted into columns in the new, output dataset. By
#    default, the function uses every column except the two columns given by
#    idvar and timevar.
#
# Returns: A dataframe with the same data as Long.data but in wide format, with
#  one row per patient and one column for each combination of times (in timevar)
#  and variables given by "vars".
long_to_wide <- function(Long.data, idvar, timevar,
                         vars=setdiff(names(Long.data), c(idvar, timevar))) {
  # Full outer join on all unique values of ID and time
  Data <- Long.data[order(Long.data[[idvar]], Long.data[[timevar]]),
                    c(idvar, timevar, vars)]
  Wide <- unique(Data[idvar])
  for (varname in vars) {
    Wide <- merge(Wide,
                  reshape(Data[c(idvar, timevar, varname)], v.names=varname,
                          timevar=timevar, idvar=idvar, direction="wide",
                          sep="_"),
                  by=idvar, all.x=TRUE)
  }
  return(Wide[order(Wide[[idvar]]),])
}




# TODO comment, and improve comments inside func
cut_clusterTree <- function(model, num.clusters) {
  # Unique cutpoints based on the "alpha" metric
  cutpoints <- sort(unique(model$alphaBottom))

  clusters <- list()
# for a given alpha, we want those clusters with 1) alpha bottom <= alpha and 2) no *children* with alphabottom <= alpha
  for (i in 1:length(cutpoints)) {
    clusters[[i]] <- which(model$alphaBottom >= cutpoints[i]
                           & (!model$id %in% model$parent
                              | model$alphaTop < cutpoints[i]))
  }

  # Pick the smallest number of clusters that is greater than or equal to the
  # requested number. (Note that it might not be possible to cut the tree
  # exactly at the requested number of clusters).
  nclust.in.cut <- vapply(clusters, length, integer(1))
  num.clusters <- min(c(max(nclust.in.cut),
                        nclust.in.cut[nclust.in.cut >= num.clusters]))
  # Pick the list of clusters that result in this number of clusters
  clusters <- model$DataPoints[clusters[[which(nclust.in.cut == num.clusters)]]]

  # Create vector of cluster numbers
  clust <- rep(as.integer(NA), length(model$density))
  for (i in 1:length(clusters)) {
    clust[clusters[[i]]] <- i
  }

# TODO: think about how to cut the tree s.t. it guarnatees every item in a cluster a la hclust; maybe even add options for k-nn classification *OR* kmeans for grouping the rest into clusters

  return (clust)
}




