#read data
train_set <- read.csv("ohms_train.csv")
test_set <- read.csv("ohms_test.csv")
#cut off classes 4 calculating distance
train_set_noclass <- train_set[,1]
test_set_noclass <- test_set[,1]
#distance method
edistance <- function(x,y,n){
  d=0
  #n is the dimensions of the vector or # of components
  for (i in 1:n){
    d=d+(x[i]-y[i])^2
  }
  d=sqrt(d)
  return(d)
}

differences <- numeric()

#test set loop
for(i in 1:nrow(test_set))
{
  act_resistance <- test_set$I[i]
  #train set loop
  distances <- numeric()
  x <- test_set_noclass[i]
  for(j in 1:nrow(train_set))
  {
    #get distances
    y <- train_set_noclass[j]
    distances[j] <- edistance(x,y,1)#it's just the one voltage measurement
  }
  #stitch distances to their classes
  results <- as.data.frame(cbind(distances, train_set$I))
  colnames(results) <- c("distance", "Resistance")
  #sort by distance
  results <- results[order(results$distance),]
  #grab 5 nearest neighbors
  cc <- results[1:5,]
  #get weighted average
  weights <- numeric()
  sumWeight = 0
  sumWeightY = 0
  for(k in 1:5)
  {
    weights[k] <- exp(-cc[k,1])
    sumWeight = sumWeight+weights[k]
    sumWeightY = sumWeightY+weights[k]*cc[k,2]
  }
  #declare predicted value
  predResist <- sumWeight/sumWeightY
  differences[i] <- abs(act_resistance-predResist)
  # print(paste("Test: ", i))
  # print(paste("Predicted resistance: ", predResist))
  # print(paste("Actual resistance: ", act_resistance))
  # print(paste("Difference: ", differences[i]))
  # print("")
  #get error/accuracy/whatever
}
#get rms
print(paste("RMS: ", mean(differences^2)))