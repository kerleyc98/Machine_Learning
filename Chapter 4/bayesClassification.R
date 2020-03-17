#bayes classification
train_set <- read.csv("insect_train.csv")
test_set <- read.csv("insect_test.csv")
#P(c1 | X1) = P(c1)P(x1|c1)
grass_abd <- hist(train_set[train_set$c == 1, ]$abd, col = rgb(1.0, 0.2, 0.75, 0.5), xlim = c(0,8), ylim = c(0,20), breaks = 10)
kat_abd <- hist(train_set[train_set$c == 2, ]$abd, col= rgb(0.2, 0.85, 0.35, 0.5), xlim = c(0,8), ylim = c(0,20), breaks = 10, add = T)
