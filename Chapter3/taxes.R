x <- c(70,60,75,85,90,95,100,120,125,220)
y <- c(1,1,1,2,2,2,1,1,1,1)
datatax <- data.frame(y,x)
datatax <- datatax[order(datatax$x),]
n <- nrow(datatax)
s <- numeric()
s[1]=55
s[n+1]=230
for (i in 2:n){
  s1 <- datatax$x[i-1]
  s2 <- datatax$x[i]
  s[i]=floor((s1+s2)/2)
}
gini <- numeric()
for (i in 1:(n+1)){
 ss <- s[i]
 nl <- length(which(datatax[,2]<=ss))
 nr <- length(which(datatax[,2]>ss))
 n1l <- length(which(datatax[,2]<=ss & datatax[,1]==1)) 
 n1r <-  length(which(datatax[,2]>ss & datatax[,1]==1))
 pl=nl/n
 pr=nr/n
 pl1=n1l/nl
 if (nl==0){pl1=0}
 pl2=1-pl1
 pr1=n1r/nr
 if (nr==0){pr1=0}
 pr2=1-pr1
 gini[i]=pl*(1-pl1^2-pl2^2)+pr*(1-pr1^2-pr2^2)
}
r <- data.frame(gini,s)
r <- r[order(r$gini),]
print(r)
rsplit=r[1,]
print(rsplit)
write.table(r,file="taxresults.csv")