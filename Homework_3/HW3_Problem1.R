data("iris")
#make test set of 30% of data, random sample
n = nrow(iris)*.3
testdata = iris[sample(nrow(iris), size = n),]
#get the probabilities for each class:
dataVers = iris[which(iris[,5]=="versicolor"),]
dataVirg = iris[which(iris[,5]=="virginica"),]
dataSeto = iris[which(iris[,5]=="setosa"),]
nVers = nrow(dataVers)
nVirg = nrow(dataVirg)
nSeto = nrow(dataSeto)
nTotal = nrow(iris)
pVers = nVers/nTotal
pVirg = nVirg/nTotal
pSeto = nSeto/nTotal
#MaKe tHE nAiVe bAyEs clAsSiFieR
#go through each test case and calculate probability for each class
#petal length: 
p1 = numeric()
p2 = numeric()
p3 = numeric()
cl = integer()
me1 = mean(dataVers$Petal.Length)
me2 = mean(dataVirg$Petal.Length)
me3 = mean(dataSeto$Petal.Length)
sd1 = sd(dataVers$Petal.Length)
sd2 = sd(dataVirg$Petal.Length)
sd3 = sd(dataSeto$Petal.Length)

n = nrow(testdata)
for(i in 1:n)
{
  z1 = (testdata$Petal.Length[i]-me1)/sd1
  z2 = (testdata$Petal.Length[i]-me2)/sd2
  z3 = (testdata$Petal.Length[i]-me3)/sd3
  f1 = dnorm(z1, 0, 1)
  f2 = dnorm(z2, 0, 1)
  f3 = dnorm(z3, 0, 1)
  p1[i] = pVers*(f1/(f1+f2+f3))
  p2[i] = pVirg*(f2/(f1+f2+f3))
  p3[i] = pSeto*(f3/(f1+f2+f3))
  proball <- cbind(p1[i], p2[i], p3[i])
  nm <- which(proball == max(proball))
  if(nm == 1)
  {
    cl[i] = "versicolor"
  } else if(nm == 2){
    cl[i] = "virginica"
  } else {
    cl[i] = "setosa"
  }
}
#calculate accuracy and error for classification via petal length
allresults <- data.frame(cl, testdata$Species, p1, p2, p3)
colnames(allresults) <- c("predict", "actual", "prob_vers", "prob_virg", "prob_seto")
n4 = nrow(allresults)
TVers = 0
TVirg = 0
TSeto = 0
FVers = 0
FVirg = 0 
FSeto = 0
for(i in 1:n4)
{
  if(allresults$predict[i] == allresults$actual[i])
  {
    if(allresults$predict[i] == "versicolor")
    {
      TVers = TVers + 1
    } else if(allresults$predict[i] == "virginica")
    {
      TVirg = TVirg + 1
    } else {
      TSeto = TSeto + 1
    }
  } else {
    if(allresults$predict[i] == "versicolor")
    {
      FVers = FVers + 1
    } else if(allresults$predict[i] == "virginica")
    {
      FVirg = FVirg + 1
    } else {
      FSeto = FSeto + 1
    }
  }
}
OA = sum(TVers, TVirg, TSeto)/n4
accVers = TVers/(TVers+FVers)
accVirg = TVirg/(TVirg+FVirg)
accSeto = TSeto/(TSeto+FSeto)
accuraciesL = cbind(OA, accVers, accVirg, accSeto)
print("Output using petal length: ")
print(allresults)
print("accuracies using petal length: ")
print(accuraciesL)
#now do it for petal width
me1 = mean(dataVers$Petal.Width)
me2 = mean(dataVirg$Petal.Width)
me3 = mean(dataSeto$Petal.Width)
sd1 = sd(dataVers$Petal.Width)
sd2 = sd(dataVirg$Petal.Width)
sd3 = sd(dataSeto$Petal.Width)

n = nrow(testdata)
for(i in 1:n)
{
  z1 = (testdata$Petal.Width[i]-me1)/sd1
  z2 = (testdata$Petal.Width[i]-me2)/sd2
  z3 = (testdata$Petal.Width[i]-me3)/sd3
  f1 = dnorm(z1, 0, 1)
  f2 = dnorm(z2, 0, 1)
  f3 = dnorm(z3, 0, 1)
  p1[i] = pVers*(f1/(f1+f2+f3))
  p2[i] = pVirg*(f2/(f1+f2+f3))
  p3[i] = pSeto*(f3/(f1+f2+f3))
  proball <- cbind(p1[i], p2[i], p3[i])
  nm <- which(proball == max(proball))
  if(nm == 1)
  {
    cl[i] = "versicolor"
  } else if(nm == 2){
    cl[i] = "virginica"
  } else {
    cl[i] = "setosa"
  }
}
#calculate overall accuracy and error yadda yadda
allresultsW <- data.frame(cl, testdata$Species, p1, p2, p3)
colnames(allresultsW) <- c("predict", "actual", "prob_vers", "prob_virg", "prob_seto")
n4 = nrow(allresultsW)
TVers = 0
TVirg = 0
TSeto = 0
FVers = 0
FVirg = 0 
FSeto = 0
for(i in 1:n4)
{
  if(allresultsW$predict[i] == allresultsW$actual[i])
  {
    if(allresultsW$predict[i] == "versicolor")
    {
      TVers = TVers + 1
    } else if(allresultsW$predict[i] == "virginica")
    {
      TVirg = TVirg + 1
    } else {
      TSeto = TSeto + 1
    }
  } else {
    if(allresultsW$predict[i] == "versicolor")
    {
      FVers = FVers + 1
    } else if(allresultsW$predict[i] == "virginica")
    {
      FVirg = FVirg + 1
    } else {
      FSeto = FSeto + 1
    }
  }
}
accVers = TVers/(TVers+FVers)
accVirg = TVirg/(TVirg+FVirg)
accSeto = TSeto/(TSeto+FSeto)
accuraciesW = cbind(OA, accVers, accVirg, accSeto)
#printout
print("Output using petal width: ")
print(allresultsW)
print("accuracies using petal width: ")
print(accuraciesW)