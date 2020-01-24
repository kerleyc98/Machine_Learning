#setwd("~/Desktop/MLnew/Chapter4")
insect <- read.csv("insects.csv",as.is=T)
#print(insect)
#grab column with label c
c <- insect$c
#find length of c 
n=length(insect$c)
#find dimensions of data insect
d=dim(insect)
#print(n)
#print(d)
#or can do this
print(paste('The number of lines of data is ',n))
print(paste('The dimesnsions of the data are ',d[1] ,d[2]))
#declare variable for new class labels
cn <- integer()
#loop that changes class g to 1 and k to 2
for (i in 1:n){
  if (c[i]=="g"){
    cn[i]=1
  }else{
    cn[i]=2
  }
}
#grab the columns labeled x and y
#which are abdomin length and antenna length
x <- insect$x
y <- insect$y
#make all values which are negative postive by absolute value
#since they are distances which is a postive quantity
x <- abs(x)
y <- abs(y)
#put them in a data frame
dataint <- data.frame(cn,x,y)
#print(dataint)
#rename the columns of the data frame
colnames(dataint) <- c("c","abd","ant")
#make csv file for data so we can view it
write.table(dataint, file = "insect_class.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
#now lets do 5-fold cross validation
#since n is 500 we will split data up into 5 randomly chosen subsets
#randomly pick 100 lines from the data
index1=sample(1:n,100,replace=F)
#put them in order from smallest index to largest index
index1=sort(index1)
#print(index1)
#make a for loop to get these values and store them
#declare an empty data frame
subdata1 <- data.frame(cl=integer(),value1=numeric(),
                       value2=numeric())
for (i in 1:100){
  #this is the index in the orginial data
  j=index1[i]
  #this grabs the jth row of the orginal data
  #and puts that row in the subset
  subdata1[i,] <- dataint[j,]
}
#rename columns again
colnames(subdata1) <- c("c","abd","ant")
#save them in csv file
write.table(subdata1, file = "insect_sub1.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
#now we need to get rid of the lines form orginal
#data that we put in subset
#make a loop for this
for (i in 100:1){
  j=index1[i]
  dataint <- dataint[-j,]
}
#make sure dataint doe not include those lines of 
#data by checking the length of one of the columns
#which we also need for next subset
n2 <- length(dataint$abd)
#can also do dimensions
d <- dim(dataint)
#print(d)
#therefore print it to check it
print(paste('This better be 400  ','and it is ',n2,'. so it is correct'))
#now just repeat process 4 more times for now!!!!!
#2nd subset
index2=sample(1:n2,100,replace=F)
index2=sort(index2)
subdata2 <- data.frame(cl=integer(),value1=numeric(),
                       value2=numeric())
for (i in 1:100){
  j=index2[i]
  subdata2[i,] <- dataint[j,]
}
colnames(subdata2) <- c("c","abd","ant")
write.table(subdata2, file = "insect_sub2.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
for (i in 100:1){
  j=index2[i]
  dataint <- dataint[-j,]
}
n3 <- length(dataint$abd)
#print(n3)
#3rd subset
index3=sample(1:n3,100,replace=F)
index3=sort(index3)
subdata3 <- data.frame(cl=integer(),value1=numeric(),
                       value2=numeric())
for (i in 1:100){
  j=index3[i]
  subdata3[i,] <- dataint[j,]
}
colnames(subdata3) <- c("c","abd","ant")
write.table(subdata3, file = "insect_sub3.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
for (i in 100:1){
  j=index3[i]
  dataint <- dataint[-j,]
}
n4 <- length(dataint$abd)
#print(n4)
#4th subset
index4=sample(1:n4,100,replace=F)
index4=sort(index4)
subdata4 <- data.frame(cl=integer(),value1=numeric(),
                       value2=numeric())
for (i in 1:100){
  j=index4[i]
  subdata4[i,] <- dataint[j,]
}
colnames(subdata4) <- c("c","abd","ant")
write.table(subdata4, file = "insect_sub4.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
for (i in 100:1){
  j=index4[i]
  dataint <- dataint[-j,]
}
#check that 5th subset only has 100
#and rename it subdata5
d <- dim(dataint)
#print(d)
subdata5 <- dataint
#print(subdata5)
d <- dim(subdata5)
write.table(subdata2, file = "insect_sub5.csv", append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
#print(d)
train <- data.frame(cl=integer(),value1=numeric(),
                     value2=numeric())
test <- data.frame(cl=integer(),value1=numeric(),
                    value2=numeric())
#train <- data.frame()
#test <- data.frame()
for (i in 1:5){
  #create the name for which subset to get
  name <- paste('insect_sub',i,'.csv',sep ="")
  for (j in 1:5){
    if (i==j){
      #since we want this subset to be the test set make it
      #equl to test by reading in the csv file
      test <- read.csv(name,as.is=T)
    }else{
      #want the rest of the subsets to be in training set
    name2 <- paste('insect_sub',j,'.csv',sep ="")
      #keep adding the lines of data by rbind which is row bind
    addtrain <- read.csv(name2,as.is=T) 
    train <- rbind(train,addtrain)
    }
  }
  #now save each training and test set in csv file
  name3 <- paste('train',i,'.csv',sep ="")
  write.table(train, file =name3, append = F, 
              quote = F, sep = ",",eol = "\n", na = "NA", 
              dec = ".", row.names = F,col.names = T)
  name4 <- paste('test',i,'.csv',sep ="")
  write.table(test, file =name4, append = F, 
              quote = F, sep = ",",eol = "\n", na = "NA", 
              dec = ".", row.names = F,col.names = T)
  #re-declare data frames to make them empty
  train <- data.frame()
  test <- data.frame()
}
#lets now check that it works
train1 <- read.csv("train1.csv",as.is=T)
train2 <- read.csv("train2.csv",as.is=T)
train3 <- read.csv("train3.csv",as.is=T)
train4 <- read.csv("train4.csv",as.is=T)
train5 <- read.csv("train5.csv",as.is=T)
test1 <- read.csv("test1.csv",as.is=T)
test2 <- read.csv("test2.csv",as.is=T)
test3 <- read.csv("test3.csv",as.is=T)
test4 <- read.csv("test4.csv",as.is=T)
test5 <- read.csv("test5.csv",as.is=T)
print(dim(train1))
print(dim(test1))
print(dim(train2))
print(dim(test2))
print(dim(train3))
print(dim(test3))
print(dim(train4))
print(dim(test4))
print(dim(train5))
print(dim(test5))
#now lets make histrograms to see the prob. distribution 
#for each class in the training sets for the ant variable
for (i in 1:5){
name <- paste('train',i,'.csv',sep ="")
train <- read.csv(name,as.is=T)  
data11 <- train[which(train[,1]==1),]
#need number of elements that are of class 1
n1 <- nrow(data11)
data12 <- train[which(train[,1]==2),]
#need number of elements that are of class 2
n2 <- nrow(data12)
xx <- train$ant  
rr=range(xx) #gives the range
low_val <- rr[1] #high value
high_val <- rr[2] #low value
step_val <- 0.5 #gives us how big are bins are
bre=seq(low_val,high_val,step_val) #gives the breaks
insect.cut1=cut(x, bre, right=F) #this cuts the data up
# and puts it in bins for class 1
insect.cut2=cut(y, bre, right=F) #cuts for class 2
insect.freq1=table(insect.cut1) #makes the freq. distribution for 1
insect.freq2=table(insect.cut2) #makes the freq. distribution for 2
freqant1 <- as.data.frame(insect.freq1) #makes a data frame
colnames(freqant1) <- c("interval","freq")
freqant2 <- as.data.frame(insect.freq2) #makes a data frame
colnames(freqant2) <- c("interval","freq")
#calculate the midpoints
x_mid <- seq(low_val+step_val/2,high_val-step_val/2,step_val)
#calculate relative frequency or probabilities
rf1 <- freqant1$freq/sum(freqant1$freq)
rf2 <- freqant2$freq/sum(freqant2$freq)
#make data.frames with mid points and rel. freq.
prob_1_insect <- data.frame(x_mid,rf1)
colnames(prob_1_insect) <- c("x","prob1")
prob_2_insect <- data.frame(x_mid,rf2)
colnames(prob_2_insect) <- c("x","prob2")
#number of rows in prob_1_insect
ntr <- nrow(prob_1_insect)
#lets save the data
dist <- cbind(prob_1_insect,prob_2_insect)
write.table(dist, file =paste('trainprob',i,'.csv',sep =""), append = F, 
            quote = F, sep = ",",eol = "\n", na = "NA", 
            dec = ".", row.names = F,col.names = T)
#makes plots to see them
pdf(paste('hist_plots_insects_ant',i,'.pdf',sep =""))
plot(prob_1_insect,xlab="ant",ylab="rel. freq.",col="red")
points(prob_2_insect,col="green")
dev.off()
}
#linear regression
#abd vs. ant
lin1 <- lm(ant ~ abd, data=train1)
s <- summary(lin1)
print(s)
#lets get the intercept and slope from lin1
# so we calculate the error or RMS
# [[]] means value array is how you can define an array
x <- array(lin1[[1]],dim=c(1,2))
#intercept is
b <- x[1,1]
print(b)
#slope is
m <- x[1,2]
print(m)
#now calculate RMS
l <- length(test1$ant)
print(l)
#lets make a linear function
linf <- function(x,b,m){
  y=x*m+b
  return(y)
}
#calculate RMS
#print(test1)
sumerror=0
for (i in 1:l){
  #actual y value
   yact <- test1[i,3]
   #actual x value
   x <- test1[i,2]
   #predicted y value
   ypred <- linf(x,b,m)
   sumerror=sumerror+(yact-ypred)^2
  }
RMS=sumerror/l
print(paste('RMS is ',RMS))
#then if want it terms of the actual units
#not units squared take square root
sRMS <- sqrt(RMS)
print(paste('the square root RMS is ',sRMS))
#lets do linear regression on all the training data
#in a more simple way and use correlation coeficient to weigh the RMS's
#declare an array 
linresult <- array(,dim=c(5,3))
sumcor=0
for (i in 1:5){
name <- paste('train',i,".csv",sep ="")
dataLR <- read.csv(name,as.is=T)
linreg <- lm(ant ~ abd, data=dataLR)
corr <- abs(cor(dataLR$ant, dataLR$abd))
x <- array(linreg[[1]],dim=c(1,2))
#intercept is
b <- x[1,1]
#slope is
m <- x[1,2]
linresult[i,1] <- m
linresult[i,2] <- b
linresult[i,3] <- corr
sumcor=sumcor + corr
}
#could just take average like so
avm <- mean(linresult[,1])
avb <- mean(linresult[,2])
print(paste('average m is ',avm,'average b is',avb))
#However lets take a weighed average based on Correlation
#coefficient
weight <- numeric()
mweight=0
bweight=0
for (i in 1:5){
  mweight=mweight + linresult[i,1]*(linresult[i,3]/sumcor)
  bweight=bweight + linresult[i,2]*(linresult[i,3]/sumcor)
}
print(paste('weight average m is ',mweight,'weight average b is',bweight))
#now lets calculate the RMS for each test set
#and get an average  RMS
RMS <- numeric()
for (i in 1:5){
  name <- paste('insect_sub',i,'.csv',sep ="")
  dataLRtest <- read.csv(name,as.is=T)
  n <- length(dataLRtest$ant)
  sumerror=0
  for (j in 1:n){
  #actual y value
  yact <- dataLRtest[j,3]
  #actual x value
  x <- dataLRtest[j,2]
  #predicted y value
  ypred <- linf(x,bweight,mweight)
  sumerror=sumerror+(yact-ypred)^2
}
RMS[i]=sumerror/n
print(paste('RMS is ',RMS[i]))
}
#average RMS
avRMS <- mean(RMS)
print(paste('average RMS is ',avRMS))

