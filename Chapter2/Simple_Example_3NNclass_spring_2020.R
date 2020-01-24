#Very Simple example class k-NN
data1 <- read.csv("simpleClass_kNN.csv")
print(data1)
n <- length(data1$x1)
print(n)
# Scatterplot
colnames(data1) <- c("cl", "x1", "x2")
ggplot(data1, aes(x=x1, y=x2)) +
  geom_point(aes(col=cl)) + 
  labs(subtitle="x1 Vs x2", 
       y="x1", 
       x="x2", 
       title="Simple_Scatterplot", 
       caption = "Source: Barry")
ggsave("Simple_scatterplot.pdf")
test1 <- c(2.5,2.5)
#get rid of class to calculate distances
data_no_class <- data1[,-1]
print(data_no_class)
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
cc <- integer()
for (i in 1:n){
   x <- data_no_class[i,]
   y <- test1
   d[i]=edistance(x,y,nc)
}
dresults <- data.frame(d,data1$cl)
colnames(dresults) <- c("d", "c")
dresults <- dresults[order(d),] #ascending order
print(dresults)
#use k=3 or 3-NN
kk=3
cc <- dresults$c[1:kk]
sum1=0
sum2=0
for (k in 1:kk){
  if (cc[k]==1){
    sum1=sum1+1
  }else{
    sum2=sum2+1
  }  
}
sumall <- cbind(sum1,sum2)
nm <- which(sumall==max(sumall))
#the corresponding class
ck <- nm
print(paste("the class is ", ck))
