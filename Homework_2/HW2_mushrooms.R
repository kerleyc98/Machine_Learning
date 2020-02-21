#print out answers for question 1 - do it on paper?
#entropy and gini index for Is Poisonous
print("1a). What is the entropy and Gini Index of Is Poisonous?")
ent = -((5/8)*log2(5/8))-(3/8*log2(3/8))
print(paste("Entropy: 5/8(log2(5/8))-3/8(log2(3/8)) = ", ent))
gini = 1-((5/8)^2)
print(paste("Gini Index: 1-(5/8)^2 = ", gini))
#Which attribute should you use for the root (isSmooth - 3 over 2)
print("1b). Which attribute should you choose as the root of a decision tree?")
print("IsSmooth, because there are 3 cases where a smooth mushroom is poisonous, but only 2 cases for every other attribute.")
print("Every other attribute has a perfect split")
#information gain for isSmooth
print("1c). What is the information gain of the attribute you chose in the previous question?")
HsmoothT = -(3/4*log2(3/4))-(1/4*log2(1/4))
HsmoothF = -(1/2*log2(1/2))-(1/2*log2(1/2))
postSplit = 1/2*(HsmoothT)+1/2*(HsmoothF)
IG = ent - postSplit
print(paste("Entropy of isSmooth = 1: ", HsmoothT))
print(paste("Entropy of isSmooth = 0: ", HsmoothF))
print(paste("Average entropy after split: ", postSplit))
print(paste("Information gain (overall entropy - average entropy after split): ", IG))
#calculate one of the nodes off of the root node (isSmooth) using ID3
print("1d). Calculate one of the nodes off of the root node you found in b using the ID3 method.")
print("Using isSpotted.")
HspottedT = -(1*log2(1)) #zero
HspottedF = -(2/3*log2(2/3))-(1/3*log2(1/3))
postSplitAgain = 1/4*(HspottedT)+3/4*(HspottedF)
IGAgain = postSplit - postSplitAgain
print(paste("Entropy of isSpotted = 1: ", HspottedT))
print(paste("Entropy of isSpotted = 0: ", HspottedF))
print(paste("Average entropy after split: ", postSplitAgain))
print(paste("Information gain: (entropy after split for isSmooth - average entropy after splitting by spotted): ", IGAgain))