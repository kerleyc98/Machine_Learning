#setwd("~/Desktop/Desktop/MLnew")
#library(nnet)
name1 <- character()
datac <- read.csv(file="cars.csv",as.is=T)
name1 <- colnames(datac)
print(name1)
datac[,1] <- as.numeric(as.factor(datac[,1]))
datac[,2] <- as.numeric(as.factor(datac[,2]))
datac[,3] <- as.numeric(as.factor(datac[,3]))
datac[,4] <- as.numeric(as.factor(datac[,4]))
datac[,5] <- as.numeric(as.factor(datac[,5]))
#datac[,1] <- sapply(datac[,1],as.numeric(as.character(datac[,1])))
n <- length(datac$engine)
#print(n)
s <- numeric()
for (i in 1:4){
  name2 <- name1[i]
  #print(name2)
#  stop()
#for (j in 1:n){
#engine_size <- ddply(datac,.(engine,fast))
n1 <- length(which(datac[,i]==1))
n2 <- length(which(datac[,i]==2))
n3 <- length(which(datac[,i]==3))
if (n3==0) {n3=1}
if (n2==0) {n2=1}
if (n1==0) {n1=1}
n11 <- length(which(datac[,i]==1 & datac[,5]==1))
n22 <- length(which(datac[,i]==2 & datac[,5]==1))
n33 <- length(which(datac[,i]==3 & datac[,5]==1))
#calculate entropy for engine
log1=log2(n11/n1)
log11=log2(1-n11/n1)
#nc <- as.integer(n11/n1)
if (n11/n1>=0.998 | n11/n1<=0.002){
  loq1=0
  log11=0
}
log21=log2(n22/n2)
log22=log2(1-n22/n2)
#nc <- as.integer(n22/n)
if (n22/n2>=0.998 | n22/n2<=0.002){
  loq21=0
  log22=0
}
log3=log2(n33/n3)
log33=log2(1-n33/n3)
#nc <- as.integer(n33/n3)
if (n33/n3>=0.998 | n33/n3<=0.002){
  log3=0
  log33=0
}
s[i]=(n1/n)*((-n11/n1)*log1-(1-(n11/n1))*log11)+
  (n2/n)*((-n22/n2)*log21-(1-(n22/n2))*log22)+
  (n3/n)*((-n33/n3)*log3-(1-(n33/n3))*log33)
print(s[i])
}
nmin <- which.min(s)
root <- name1[nmin]
print(root)
leaf1 <- integer()
root1 <- character()
nmin2 <- integer()
nmin2[1]=0
nmin3=0
ss <- numeric()  
for (j in 1:3){
 
for (i in 1:4){
  name2 <- name1[i]
  print(name2)
  print(j)
  if (j >=2){
    jj=j-1
  nmin3 <- nmin2[jj]
  }else {
  nmin3 <- nmin2[1]
  }
  #print(nmin3)
  if (i==nmin | i==nmin3){
    print("do nothing")
  }else{
  #  stop()
  #for (j in 1:n){
  #engine_size <- ddply(datac,.(engine,fast))
  n1 <- length(which(datac[,i]==1 & datac[,nmin]==j))
  n2 <- length(which(datac[,i]==2 & datac[,nmin]==j))
  n3 <- length(which(datac[,i]==3 & datac[,nmin]==j))
  if (n3==0) {n3=1}
  if (n2==0) {n2=1}
  if (n1==0) {n1=1}
  n11 <- length(which(datac[,i]==1 & datac[,5]==1 & datac[,nmin]==j))
  n22 <- length(which(datac[,i]==2 & datac[,5]==1 & datac[,nmin]==j))
  n33 <- length(which(datac[,i]==3 & datac[,5]==1 & datac[,nmin]==j))
  #calculate entropy 
  log1=log2(n11/n1)
  log11=log2(1-n11/n1)
  #print(n11/n1)
  #nc <- as.integer(n11/n1)
  if (n11/n1>=0.995 | n11/n1<=0.005){
    log1=0
    log11=0
    
  }
  #print(log1)
  #print(log11)
  #print(n11/n1)

  log21=log2(n22/n2)
  log22=log2(1-n22/n2)
  #nc <- as.integer(n22/n)
  if (n22/n2>=0.995 | n22/n2<=0.005){
    log21=0
    log22=0
  }
  #print(log21)
  #print(log22)
  #print(n22/n2)
  log3=log2(n33/n3)
  log33=log2(1-n33/n3)
  #nc <- as.integer(n33/n3)
  if (n33/n3>=0.995 | n33/n3<=0.005){
    log3=0
    log33=0
  }
  #print(log3)
  #print(log33)
  #print(n33/n3)
  ss[i]=(n1/n)*((-n11/n1)*log1-(1-(n11/n1))*log11)+
    (n2/n)*((-n22/n2)*log21-(1-(n22/n2))*log22)+
    (n3/n)*((-n33/n3)*log3-(1-(n33/n3))*log33)
  print(ss[i])

  }
}
if (ss[1]==0 & ss[2]==0 & ss[3]==0){
  leaf1[j]=j
  nmin2[j]=0
}else{
nmin2[j] <- which.min(ss)
nmin3 <- nmin2[j]
root1[j] <- name1[nmin3]
}
}
rootleafs <- data.frame(root,leaf1[1],root1[2],root1[3])
colnames(rootleafs) <- c("root", "leaf1","root2","root3")
write.table(rootleafs, file = "rootleaf.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
