#Tree final project
#import tree
library(tree)

#Create Tree
tree1 <- tree(IsAcceptedAnswer~qa$AnswerScore + qa$ContainsImage + qa$ContainsCode + qa$ContainsLink + qa$ResponseTimeHours, data=qa)
#Print Tree
plot(tree1)
text(tree1)