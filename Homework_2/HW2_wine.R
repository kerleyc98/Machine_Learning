#build a decision tree using rpart
library(rpart)
winetrain <- read.csv("winedata.csv")
#shamelessly stolen from the rpart_in_lession script
cfit <- rpart(as.factor(c)~.,data=winetrain,method="class")
name1="decisionTREE_wine"
num=1
ext=".pdf"
name2=paste(name1,num,ext,sep='')
pdf(name2)
plot(cfit,uniform=T,main="Decision Tree for Wine data")
text(cfit,use.n=T,all=T,cex=.6)
dev.off()
pred <- predict(cfit,data=winetest,type="class")
t <- table(winetrain$c,pred)
print(t)
#run test data through decision tree