#build a decision tree using rpart
library(rpart)
winetrain <- read.csv("winedata.csv")
#shamelessly stolen from the rpart_in_lession script
cfit <- rpart(as.factor(c)~.,data=winetrain,method="class")
name1="decision_tree_wine"
num=1
ext=".pdf"
name2=paste(name1,num,ext,sep='')
pdf(name2)
plot(cfit,uniform=T,main="Decision Tree for Wine data")
text(cfit,use.n=T,all=T,cex=.6)
dev.off()
#run test data through decision tree
winetest <- read.csv("winedata_test.csv")
pred <- predict(cfit,data=winetest,type="class")
t <- table(winetrain$c,pred)
print(t)
d <- sum(diag(t))
n <- sum(t)
overall_ac=d/n
acc1 = t[1,1]/sum(t[1,])
acc2 = t[2,2]/sum(t[2,])
acc3 = t[3,3]/sum(t[3,])
print(paste("Overall accuracy: ", overall_ac))
print(paste("Accuracy of class 1: ", acc1))
print(paste("Accuracy of class 2: ", acc2))
print(paste("Accuracy of class 3: ", acc3))

print("Does the decision tree contain all the features?")
print("no")
print("What are the important features in this data set?")
print("Proline, flavanoids od289_OD315_of_diluted_wines, and hue are important")
print("Might be because these features are the most distinguishable between classes")
