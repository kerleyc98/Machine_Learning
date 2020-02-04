#read data
irises <- data("iris")
n <- 50
#clip class for distances
#sample training set
train_set = irises[sample(nrow(irises), size = 50),]
train_set_noclass <- train_set[,-1] #figure out how to fix this b/c the class comes at the end of the row instead of the beginning
#sample test set
test_set = irises[sample(nrow(irises), size = 50),]
test_set_noclass <- test_set[, -1]
#plotting the thing is impossible
#distance method (euclidian)
edistance <- function(x,y,n){
  d=0
  #n is the dimensions of the vector or # of components
  for (i in 1:n){
    d=d+(x[i]-y[i])^2
  }
  d=sqrt(d)
  return(d)
}
#loop for each point in test set
for (j in 1:n){ #test rows
  #make array for distances
  distances = numeric()
  #calc distances and store in array
  nc = ncol(train_set_noclass)
  y <- test_set_noclass[j,]
  for (m in 1:n){#train rows
    x <- train_set_noclass[m,]
    distances[m] <- edistance(x, y, nc)
  }
  #stitch distances to class
  results = data.frame(distances, train_set$Species)
  colnames(results) <- c("distance", "species")
  #sort results
  results <- results[order(distances),]
  #take top k entries
  kk <- 3 #3 nearest neighbors
  cc <- results$species[1:kk]
  #tally classes
  sumSet=0
  sumVer=0
  sumVir=0
  for (k in 1:kk){
    if(cc[k] == "setosa"){
      sumSet = sumSet+1
    } else if(cc[k] == "versicolor"){
      sumVer = sumVer+1
    } else {
      sumVir = sumVir+1
    }
  }
  #declare classification
  #stitch together sums and use which.max
  
  #sum tp/fp/tn/fn
}
#somehow find out how to form confusion matrix
#donezo