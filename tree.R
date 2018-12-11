#Tree final project
#import tree
library(tree)

#Load the qa dataset after running the data cleaning script on raw data

#Create Tree
tree1 <- tree(IsAcceptedAnswer~qa$AnswerScore + qa$ContainsImage + qa$ContainsCode + qa$ContainsLink + qa$ResponseTimeHours, data=qa)
#Print Tree
plot(tree1)
text(tree1)