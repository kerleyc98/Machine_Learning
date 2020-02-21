library(rpart)
data(iris)
cfit <- rpart(as.factor(Species)~.,data=iris,method="class")
name1="decisionTREE_iris"
num=1
ext=".pdf"
name2=paste(name1,num,ext,sep='')
pdf(name2)
plot(cfit,uniform=T,main="Decision Tree for Iris data")
text(cfit,use.n=T,all=T,cex=.6)
dev.off()
pred <- predict(cfit,data=iris,type="class")
t <- table(iris$Species,pred)
print(t)
stop()
d <- diag(t)
n <- sum(t)
overall_ac=d/n
n1 <- sum(t[1,])
ns <- t[1,1]
Acc1=ns/n1
print(Acc1)
n2 <- sum(t[2,])
ns <- t[2,2]
print(ns/n2)
#regression
rfit <- rpart(Sepal.Length ~ Sepal.Width + Petal.Length + 
                Petal.Width, method="anova",data=iris)
plot(rfit,uniform=T,main="regression for iris")
text(rfit,use.n=T,cex=.6)
p <- predict(rfit,iris,method="anova")
print(p)
