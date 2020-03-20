#HW3 Problem 2
#should be way easier than problem 1
data("iris")
testdata <- iris[sample(nrow(iris), size = 45),]

#classify using gaussian bayes instead of dumb useless not-explained-at-all frequency bayes
#needs sd and mean to do the gross exponent
expon <- function(mu, stddev, x)
{
  partOne <- 1/(sqrt(2*pi)*stddev)
  partTwo <- -(1/2)*(((x-mu)/stddev)^2)
  p <- partOne*exp(partTwo)
  return(p)
}
mvn <- function(mu, cov, x)
{
  partOne <- 1/((2*pi)*(det(cov)^(1/2)))
  partTwo <- (-1/2)*(t(x-mu) %*% (cov^-1) %*% (x-mu))
  p <- partOne*exp(partTwo)
}
dataVers = iris[which(iris[,5]=="versicolor"),]
dataVirg = iris[which(iris[,5]=="virginica"),]
dataSeto = iris[which(iris[,5]=="setosa"),]

pVe <- nrow(dataVers)/nrow(iris)
pVi <- nrow(dataVirg)/nrow(iris)
pSe <- nrow(dataSeto)/nrow(iris)

muVersL = mean(dataVers$Petal.Length)
muVersW = mean(dataVers$Petal.Width)
muVirgL = mean(dataVirg$Petal.Length)
muVirgW = mean(dataVirg$Petal.Width)
muSetoL = mean(dataSeto$Petal.Length)
muSetoW = mean(dataSeto$Petal.Width)
sdVersL = sd(dataVers$Petal.Length)
sdVersW = sd(dataVers$Petal.Width)
sdVirgL = sd(dataVirg$Petal.Length)
sdVirgW = sd(dataVirg$Petal.Width)
sdSetoL = sd(dataSeto$Petal.Length)
sdSetoW = sd(dataSeto$Petal.Width)
#go through test data and plug into the above function
p1 <- numeric()
p2 <- numeric()
p3 <- numeric()
cl <- integer()
n4 <- nrow(testdata)
for(i in 1:n4)
{
  tempL <- testdata$Petal.Length[i]
  tempW <- testdata$Petal.Width[i]
  pVersL = expon(muVersL, sdVersL, tempL)
  pVersW = expon(muVersW, sdVersW, tempW)
  pVirgL = expon(muVirgL, sdVirgL, tempL)
  pVirgW = expon(muVirgW, sdVirgW, tempW)
  pSetoL = expon(muSetoL, sdSetoL, tempL)
  pSetoW = expon(muSetoW, sdSetoW, tempW)
  p1[i] = pVe*pVersL*pVersW
  p2[i] = pVi*pVirgL*pVirgW
  p3[i] = pSe*pSetoL*pSetoW
  proball <- cbind(p1[i], p2[i], p3[i])
  nm <- which(proball == max(proball))
  if(nm == 1)
  {
    cl[i] = "versicolor"
  } else if(nm == 2)
  {
    cl[i] = "virginica"
  } else {
    cl[i] = "setosa"
  }
}
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
accuracies = cbind(OA, accVers, accVirg, accSeto)
print("output using petal length and width independently: ")
print(allresults)
print("accuracies using petal length and width independently: ")
print(accuracies)
#now do it with a multivariate gaussian function
#whatever that means
#these are for making the covariance matrices
muVers <- c(muVersL, muVersW)
muVirg <- c(muVirgL, muVirgW)
muSeto <- c(muSetoL, muSetoW)
#covariance for versicolor
coVers <- matrix(0.0, nrow = 2, ncol = 2)
covVers <- matrix(0.0, nrow = 2, ncol = 2)
for(i in 1:nrow(dataVers))
{
  #make the vector for X (length and width)
  x <- dataVers$Petal.Length[i]
  y <- dataVers$Petal.Width[i]
  v <- c(x,y)
  #X-mu
  d <- v-muVers
  #(x-mu)(x-mu)^T
  coVers <- d %o% d
  #sum
  covVers = covVers + coVers
}
covVers = covVers*(1/nrow(dataVers))
#covariance for virginica
coVirg <- matrix(0.0, nrow = 2, ncol = 2)
covVirg <- matrix(0.0, nrow = 2, ncol = 2)
for(i in 1:nrow(dataVirg))
{
  #make the vector for X (length and width)
  x <- dataVirg$Petal.Length[i]
  y <- dataVirg$Petal.Width[i]
  v <- c(x,y)
  #X-mu
  d <- v-muVirg
  #(x-mu)(x-mu)^T
  coVirg <- d %o% d
  #sum
  covVirg = covVirg + coVirg
}
covVirg = covVirg*(1/nrow(dataVirg))
#covariance for setosa
coSeto <- matrix(0.0, nrow = 2, ncol = 2)
covSeto <- matrix(0.0, nrow = 2, ncol = 2)
for(i in 1:nrow(dataSeto))
{
  #make the vector for X (length and width)
  x <- dataSeto$Petal.Length[i]
  y <- dataSeto$Petal.Width[i]
  v <- c(x,y)
  #X-mu
  d <- v-muSeto
  #(x-mu)(x-mu)^T
  coSeto <- d %o% d
  #sum
  covSeto = covSeto + coSeto
}
covSeto = covSeto*(1/nrow(dataSeto))
#use det() for determinants and t() for transpose
#classify
MpVers <- numeric()
MpVirg <- numeric()
MpSeto <- numeric()
mcl <- integer()
n4 <- nrow(testdata)
for(i in 1:n4)
{
  x <- testdata$Petal.Length[i]
  y <- testdata$Petal.Width[i]
  v <- c(x,y)
  #versicolor
  MpVers[i] <- mvn(muVers, covVers, v)
  MpVirg[i] <- mvn(muVirg, covVirg, v)
  MpSeto[i] <- mvn(muSeto, covSeto, v)
  
  Mproball <- c(MpVers[i], MpVirg[i], MpSeto[i])
  nm <- which(Mproball == max(Mproball))
  if(nm == 1)
  {
    mcl[i] = "versicolor"
  } else if(nm == 2)
  {
    mcl[i] = "virginica"
  } else {
    mcl[i] = "setosa"
  }
}
#accuracies
Mallresults <- data.frame(mcl, testdata$Species, MpVers, MpVirg, MpSeto)
colnames(Mallresults) <- c("predict", "actual", "prob_vers", "prob_virg", "prob_seto")
n4 = nrow(Mallresults)
MTVers = 0
MTVirg = 0
MTSeto = 0
MFVers = 0
MFVirg = 0 
MFSeto = 0
for(i in 1:n4)
{
  if(Mallresults$predict[i] == Mallresults$actual[i])
  {
    if(Mallresults$predict[i] == "versicolor")
    {
      MTVers = MTVers + 1
    } else if(Mallresults$predict[i] == "virginica")
    {
      MTVirg = MTVirg + 1
    } else {
      MTSeto = MTSeto + 1
    }
  } else {
    if(Mallresults$predict[i] == "versicolor")
    {
      MFVers = MFVers + 1
    } else if(Mallresults$predict[i] == "virginica")
    {
      MFVirg = MFVirg + 1
    } else {
      MFSeto = MFSeto + 1
    }
  }
}
MOA = sum(MTVers, MTVirg, MTSeto)/n4
MaccVers = MTVers/(MTVers+MFVers)
MaccVirg = MTVirg/(MTVirg+MFVirg)
MaccSeto = MTSeto/(MTSeto+MFSeto)
Maccuracies = cbind(MOA, MaccVers, MaccVirg, MaccSeto)
#printout
print("Output for using multivariate gaussian function: ")
print(Mallresults)
print("Accuracies for multivariate gaussian function: ")
print(Maccuracies)
#Are the accuracies different? why or why not?
print("The accuracies between the independent and multivariate gaussian functions differs a little bit with the multivariate slightly more accurate using the same test data.")