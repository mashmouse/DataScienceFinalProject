#Tree final project
#import tree
library(tree)

#Create Tree
train <- sample(1:nrow(qa), 10000)
tree1 <- tree(IsAcceptedAnswer~qa$ContainsImage + qa$ContainsCode + qa$AnswerScore + qa$ContainsLink + qa$ResponseTimeHours + qa$AnswerBodyTextLength + qa$QAwordIntersection, data=qa, method="class", subset=train)
predict1 <- round(predict(tree1))
predict1 <- as.data.frame(predict1)
tree1table <- c(qa, predict1)
plot(tree1)
text(tree1, pretty = 0)
tree.pred <- predict(tree1, qa[-train,], type = "class")
with (qa[-train,], table(tree.pred, qa$IsAcceptedAnswer))