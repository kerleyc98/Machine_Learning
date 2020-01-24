#Very Simple example class k-NN
library(tidyverse)
data1 <- read.csv("SimpleRegression_kNN.csv",as.is=T)
#print(data1)
n <- length(data1$x1)
#print(n)
test1 <- c(.95,2.6)
#normalize data using z-score
meanx1 <- mean(data1$x1)
meanx2 <- mean(data1$x2)
sdx1 <- sd(data1$x1)
sdx2 <- sd(data1$x2)
print(paste("mean x1 ",meanx1," mean x2 ",meanx2))
print(paste("sd x1 ",sdx1," sd x2 ",sdx2))
for (i in 1:n){
  data1[i,2] <- (data1[i,2]-meanx1)/sdx1
  data1[i,3] <- (data1[i,3]-meanx2)/sdx2
}
#print(data1)
#need to normilze test point
test1[1] <- (test1[1]-meanx1)/sdx1
test1[2] <- (test1[2]-meanx2)/sdx2
#print(test1)
#get rid of y to calculate distances
data_no_class <- data1[,-1]
#print(data_no_class)
#now to kNN 
# distance function
edistance <- function(x,y,n){
  d=0
  #n is the dimensions of the vector or # of components
  for (i in 1:n){
    d=d+(x[i]-y[i])^2
  }
  d=sqrt(d)
  return(d)
}
#now loop over examples 
#to find distances from single test point
#declare an array for distance
nc <- ncol(data_no_class)
d <- numeric()
#declare an array to hold the top k neighbors
cc <- numeric()
for (i in 1:n){
  x <- data_no_class[i,]
  y <- test1
  d[i]=edistance(x,y,nc)
}
dresults <- data.frame(d,data1$y)
colnames(dresults) <- c("d", "y")
dresults <- dresults[order(d),] #ascending order
#print(dresults)
#use k=3 or 3-NN
kk=3
cc <- dresults$y[1:kk]
#use average to find y
ypred <- mean(cc)
print(paste("The predicted y value using average is ",ypred))
#use weighted average with weighting function exp(-x)
nw <- length(dresults$y)
#array for weights
w <- numeric()
sumw=0
sumwy=0
for (i in 1:kk){
  w[i] <- exp(-dresults[i,1])
  #print(dresults[i,1])
  #print(w[i])
  sumw=sumw + w[i]
  sumwy=sumwy + w[i]*dresults[i,2]
}
#print(w)
ypredw <- sumwy/sumw
print(paste("The predicted y value using a weighted average is ",ypredw))
