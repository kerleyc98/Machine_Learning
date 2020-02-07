#read data
data("iris")
n <- 50
#clip class for distances
#sample training set
train_set = iris[sample(nrow(iris), size = 50),]
train_set_noclass <- train_set[,1:4]
#sample test set
test_set = iris[sample(nrow(iris), size = 50),]
test_set_noclass <- test_set[, 1:4]
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
confusion_matrix <- matrix(0L, nrow = 3, ncol = 3)
correctpred=0
#loop for each point in test set
for (j in 1:n){ #test rows
  #take test row species for later comparison
  actual_species <- test_set$Species[j]
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
  results <- data.frame(cbind(distances, train_set$Species))
  results <- as.data.frame(lapply(results, unlist))
  colnames(results) <- c("distance", "train_species")
  #sort results
  results <- results[order(results$distance),]
  #take top k entries
  kk <- 3 #3 nearest neighbors
  cc <- results$train_species[1:kk]
  #tally classes
  sumSet=0
  sumVer=0
  sumVir=0
  for (k in 1:kk){
    if(cc[k] == 1){
      sumSet = sumSet+1
    } else if(cc[k] == 2){
      sumVer = sumVer+1
    } else {
      sumVir = sumVir+1
    }
  }
  #declare classification
  #stitch together sums and use which.max
  print(paste("Test: ", j))
  # print(paste("Setosa: ", sumSet))
  # print(paste("Versicolor: ", sumVer))
  # print(paste("Virginica: ", sumVir))
  sumall <- cbind(sumSet, sumVer, sumVir, actual_species)
  print(sumall)
  pred <- which(sumall[,1:3] == sumall[,sumall[,4]])
  if(pred==sumall[,4])
  {
    #increment true(species)
    correctpred=correctpred+1
    print("correct prediction")
  } else {
    #increment false(species)
  }
  #sum tp/fp/tn/fn or whatever they are for this situation
  confusion_matrix[sumall[1,4], pred] = confusion_matrix[sumall[1,4], pred]+1
}
#break down confusion matrix
#accuracy
acc_all = correctpred/50
print(paste("Overall accuracy: ", acc_all))
#accuracy of each class
acc_set = confusion_matrix[1,1]/sum(confusion_matrix[1,])
acc_ver = confusion_matrix[2,2]/sum(confusion_matrix[2,])
acc_vir = confusion_matrix[3,3]/sum(confusion_matrix[3,])
print(paste("Setosa accuracy: ", acc_set))
print(paste("Versicolor accuracy: ", acc_ver))
print(paste("Virginicolor accuracy: ", acc_vir))
#error
print(paste("Overall error: ", 1-acc_all))
#donezo