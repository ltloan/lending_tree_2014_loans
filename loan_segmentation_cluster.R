#install.packages('doBy')
#install.packages('fpc')
#2D Visualization Guide: http://stats.stackexchange.com/questions/31083/how-to-produce-a-pretty-plot-of-the-results-of-k-means-cluster-analysis

library(doBy)
library(fpc)
library(rgl)
library(plyr)

#### KMEANS APPROACH: INSUFFICIENT

#Performs clusters for 2:15 clusters and plots the results to use the "Elbow Method" against
display_cluster_sizes <- function(mydata, gTitle='K-Means Elbow Plot') { 
  set.seed(1234)
  # Determine number of clusters
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
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

file_data <- read.csv("/Users/aowens/Downloads/cleandata_roi.csv", stringsAsFactors=FALSE)

#Selection of core KPIs in numeric format
f <- file_data[,c('pctROI','funded_amnt','annual_inc')]
f <- normalize(f)

#plot the elbow curve
display_cluster_sizes(f)

#generate the best model
model1 <- generate_best_cluster_model(f,totalCls=5)

#get the model details
model1
summary(model1)

#write the clusters back to the initial dataframe for EDA purposes
file_data$segment <- model1$cluster

#Perform the PCA
mydata.pca1 <- princomp(f, cor=T, scores=T)

#Generate the 3d Plot of the clusters by the first three factors - On Mac: X11 is Required
plot3d(mydata.pca1$scores[,1:3], col=as.factor(model1$cluster), main='kmeans clusters for k=5 clusters')

plot(mydata.pca1, "Explained Variance by Principle Component Used in Visualization")

#### SUMMARY STATISTICS & VISUALIZATIONS BY SEGMENT: 

#Frequency Distritbution of the Clusters:
count(file_data, 'segment')
plot(count(file_data, 'segment'))

###PULLS DATA FOR THE CORE KPIs
#Lender Return Rate on Loan
#Borrower Default Rate on Loan
#Average Return Amount
#Average Loan Term

#% ROI by Segment
boxplot(pctROI~segment,data=file_data, main="% ROI Distribution by Segment", xlab="Segment")
#Max ROI by Segment
boxplot(maxROI~segment,data=file_data, main="Max ROI Distribution by Segment", xlab="Segment")
#Return amount
file_data$amt_rtrn <- file_data$total_pymnt-file_data$total_rec_prncp
boxplot(amt_rtrn~segment,data=file_data, main="Total Return Amount Distribution by Segment", xlab="Segment",outline=FALSE)

#loan term averages
barx <- barplot(ddply(file_data,~segment,summarise,mean=mean(term),sd=sd(term))[,c('mean')],xlab='Borrower Segment',main='Mean Loan Term by Borrower Segment',names=c("1","2","3","4","5"))
