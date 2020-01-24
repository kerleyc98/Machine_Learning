setwd("~/Desktop/Desktop/MLnew")
library(class)
acc <- numeric()
err <- numeric()
n <- integer()
datatr <- read.csv(file="pendigits_tra.csv",as.is=TRUE)
datate <- read.csv(file="pendigits_tes.csv",as.is=TRUE)
for (i in 1:100) {
n[i]=i
knear <- knn(datatr,datate,datatr$digit 
             ,k=i)
  #confusion matrix
  t <- table(datate[,'digit'],knear)
  #get diagonal elements of matrix
  td <- diag(t)
  #sum all the elements of the matrix t which gives 
  #the examples tested
  sumt <- sum(t)
  #sum the diagonal to see how many we got correct
  sumtd <- sum(td)
  #calculate accuracy
  acc[i] <- sumtd/sumt
  #calculate error
  err[i] <- (sumt-sumtd)/sumt
  #print(acc[i])
  #print(err[i])
 
}
resultsknn <- cbind(n,acc,err)
colnames(resultsknn) <- c("k","Accuracy","Error")
pdf("kNNaccuracy.pdf")
plot(1:100,acc,xlab="k",ylab="Accuracy")
dev.off()
pdf("kNNerror.pdf")
plot(1:100,err,xlab="k",ylab="Error")
dev.off()
write.table(resultsknn, file = "ResultsDigitKNN.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
#iris data
data(iris)
#easy way to split into training set and test set into 70% training
#30% test 
idxs <- sample(1:nrow(iris),as.integer(0.7*nrow(iris)))
trainIris <- iris[idxs,]
testIris <- iris[-idxs,]
class1acc <- numeric()
class2acc <- numeric()
class3acc <- numeric()
for (i in 1:100) {
  n[i]=i
  knn3 <- knn(trainIris[,-5],testIris[,-5],
              trainIris$Species ,k=i)
  # The resulting confusion matrix
  t <- table(testIris[,'Species'],knn3)
  #Calculate accuracy for each class
  t1 <- sum(t[1,])
  t11=t[1,1]
  class1acc[i]=t11/t1
  #print(class1acc)
  class2acc[i]=t[2,2]/sum(t[2,])
  #print(class2acc)
  class3acc[i]=t[3,3]/sum(t[3,])
  #print(class3acc)
  #get diagonal elements of matrix
  td <- diag(t)
  #sum all the elements of the matrix t which gives 
  #the examples tested
  sumt <- sum(t)
  #some the diagonal to see how many we got correct
  sumtd <- sum(td)
  #calculate accuracy
  acc[i] <- sumtd/sumt
  #calculate error
  err[i] <- (sumt-sumtd)/sumt
  #print(acc[i])
  #print(err[i])
  
}
resultsknn <- cbind(n,acc,err)
colnames(resultsknn) <- c("k","Accuracy","Error")
pdf("kNNaccuracyIris.pdf")
plot(1:100,acc,xlab="k",ylab="Accuracy")
dev.off()
pdf("kNNerrorIris.pdf")
plot(1:100,err,xlab="k",ylab="Error")
dev.off()
allresultsknn <- cbind(n,acc,err,class1acc,class2acc,class3acc)
colnames(allresultsknn) <- c("k","Accuracy","Error",
                          "class 1","class 2",
                          "class 3")
write.table(allresultsknn, file = "ResultsIrisKNN.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)

