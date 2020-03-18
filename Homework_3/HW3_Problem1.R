data("iris")
#make test set of 30% of data, random sample
n = nrow(iris)*.3
testdata = iris[sample(nrow(iris), size = n),]
#MaKe tHE nAiVe bAyEs clAsSiFieR
#find appropriate middle ground to classify by P(class | petal length = some number)
#make frequency distribution? who knows
#go through each test case and calculate probability for each class
#P(versicolor | petal lengt)
#calculate overall accuracy and error yadda yadda