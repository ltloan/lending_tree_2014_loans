#install.packages('doBy')
#install.packages('fpc')
#2D Visualization Guide: http://stats.stackexchange.com/questions/31083/how-to-produce-a-pretty-plot-of-the-results-of-k-means-cluster-analysis

library(doBy)
library(fpc)
library(rgl)

file_data <- read.csv("/Users/aowens/Downloads/cleandata_roi.csv", stringsAsFactors=FALSE)

#Performs clusters for 2:15 clusters and plots the results to use the "Elbow Method" against
display_cluster_sizes <- function(mydata, gTitle='K-Means Elbow Plot') { 
  set.seed(1234)
  # Determine number of clusters
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",
       main=gTitle) 
}

#Selects the "best" model
generate_best_cluster_model <- function(mydata, totalCls=3, scaleFeats=FALSE) {
  #   set.seed(1234)
  if (scaleFeats)
    mydata <- scale(mydata)
  results <- lapply(rep(25, 4), 
                    function(nstart) kmeans(mydata, totalCls, 
                                            nstart=nstart))
  i <- sapply(results, function(result) result$tot.withinss)
  result <- results[[which.min(i)]]
  return(result)
}

#Normalizes each variable to make them all comparable on a single scale
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Provides a summary description of the cluster
show_cluster_summary <- function(df, varInForm) {
  fmla <- as.formula(paste(c(varInForm, 'cluster'), collapse='~'))
  summaryBy(data=df, fmla, FUN=c(mean,sd,median))
}

#Remove the first column from the clustered data
mydata <- subset(file_data, select=-Page)

#Normalize the dataset
scaled_data <- normalize(mydata)
model1 <- generate_best_cluster_model(scaled_data)

#Perform the PCA
scaled_data.pca1 <- princomp(scaled_data, cor=T, scores=T)

#Generate the 3d Plot of the clusters by the first three factors - On Mac: X11 is Required
plot3d(scaled_data.pca1$scores[,1:3], col=as.factor(model1$cluster), 
       main='kmeans clusters for ___')