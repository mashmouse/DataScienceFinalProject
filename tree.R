#Tree final project
#import tree
library(tree)

#Create Tree
tree1 <- tree(IsAcceptedAnswer~qa$ContainsImage + qa$ContainsCode + qa$AnswerScore + qa$ContainsLink + qa$ResponseTimeHours + qa$AnswerBodyTextLength + qa$QAwordIntersection, data=qa)
#Print Tree
plot(tree1)
text(tree1)