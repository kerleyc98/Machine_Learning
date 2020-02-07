library(class)
winetrain <- read.csv("winedata.csv")
winetest <- read.csv("winedata_test.csv")
actual_class <- as.numeric(winetest$c)
final_res <- data.frame()
for(i in 1:100)
{
  #run stock knn
  res <- knn(winetrain, winetest, winetrain$c, k = i)
  #assess error/accuracy
  res_classification <- as.numeric(res)
  difference <- actual_class-res_classification
  err=0
  acc=0
  for(j in 1:length(difference))
  {
    if(difference[j] != 0)
    {
      #error
      err=err+1
    } else {
      #accuracy
      acc=acc+1
    }
  }
  # print(paste("Test: ", i))
  # print(paste("Error%: ", err/24))
  # print(paste("Accuracy: ", acc/24))
  # print("")
  final_res[i,1] <- err/24
  final_res[i,2] <- acc/24
}
colnames(final_res) <- c("Error", "Accuracy")
print("As k increases, the error increases because the majority of neighbors ends up being the incorrect class.")
print("Specifically, test entries that are near the decision border between two classes are liable to be classified incorrectly.")