#do the things with the bugs i dunno dude
bugs <- read.csv("insects_data.csv")
n = nrow(bugs)
s <- numeric()
print("The splits range from 0.8 to 9.2")
s[1]=0.6
s[n+1]=9.4
for (i in 2:n){
  s1 <- bugs$Antenna_Length[i-1]
  s2 <- bugs$Antenna_Length[i]
  s[i]=floor((s1+s2)/2)
}
#find the optimal binary split using the antenna length feature w/ the gini index
#The range of splits is 0.8 to 9.2
gini <- numeric()
for(i in 1:(n+1))
{
  ss <- s[i]
  nl <- length(which(bugs[,3]<=ss))
  nr <- length(which(bugs[,3]>ss))
  n1l <- length(which(bugs[,3]<=ss & bugs[,1]==1)) 
  n1r <-  length(which(bugs[,3]>ss & bugs[,1]==1))
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
print("Gini indices for splits:")
r <- data.frame(gini,s)
r <- r[order(r$gini),]
print(r)
rsplit=r[1,]
print("Optimal binary split: ")
print(rsplit)