#HW3, Problem 4
#make a covariance matrix outta this:
y1 <- c(64.0, 580.0, 29.0)
y2 <- c(66.0, 570.0, 33.0)
y3 <- c(68.0, 590.0, 37.0)
y4 <- c(69.0, 660.0, 46.0)
y5 <- c(73.0, 600.0, 55.0)
rawdata <- rbind(y1, y2, y3, y4, y5)
rawdata <- as.data.frame(rawdata)
colnames(rawdata) <- c("x1", "x2", "x3")
#make a mean vector
mu <- c(mean(rawdata$x1), mean(rawdata$x2), mean(rawdata$x3))
co <- matrix(0.0, nrow = 3, ncol = 3)
cov <- matrix(0.0, nrow = 3, ncol = 3)
for(i in 1:nrow(rawdata))
{
  x1 <- rawdata$x1[i]
  x2 <- rawdata$x2[i]
  x3 <- rawdata$x3[i]
  v <- c(x1, x2, x3)
  d <- v-mu
  co <- d %o% d
  cov = cov + co
}
cov = cov*(1/nrow(rawdata))
colnames(cov) <- c("x1", "x2", "x3")
rownames(cov) <- c("x1", "x2", "x3")