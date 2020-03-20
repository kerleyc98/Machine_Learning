#HW3_Problem3
#do the thing but with a package this time
data("iris")
library(e1071)
idxs <- sample(1:nrow(iris),as.integer(0.7*nrow(iris)))
trainiris <- iris[idxs,3:5]
testiris <- iris[-idxs,3:5]
x1=trainiris[,-3]
y1=trainiris$Species
x2=testiris[,-3]
y2=testiris$Species
NBclass <- naiveBayes(x1,y1,data=trainiris)
NB_pred <- predict(NBclass, testiris, type="class")
t <- table(NB_pred, testiris$Species,dnn=c("Prediction","Actual"))
print(t)
