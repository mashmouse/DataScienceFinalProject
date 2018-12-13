# Statistical Data Analysis

#Introductory Analysis of data
#  - What percentage of all answers are upvoted? 
#  - What percentage of all questions have (asker accepted) answers?
#  - What are the most popular tags on questions with accepted answers?
#  - Answers of which tag are the most successful? The least successful?
#  - Do higher answer scores and answer acceptance correlate?

library(data.table)
library(ggplot2)

# raw data (for simplicity with early questions)
answers = read.csv("~/Downloads/rquestions/answers.csv")
questions = read.csv("~/Downloads/rquestions/questions.csv")
# upload the cleaned csv R questions data
qa = read.csv("~/Desktop/Data\ Science/DataScienceFinalProject/cleanData.csv")

### what are the most popular tags on questions with accepted answers?
Tags = data.table(tags)
Tags = Tags[order(Count, decreasing = TRUE)]
head(Tags)
# The most popular tags are the basic ones. It makes sense that simpler questions
#that are asked more often will have more accepted answers, so it is logical to
#assume that these most popular tags will also be most popular on questions with 
#accepted answers.

### answers of which tag are the most successful? The least successful?
# Considering easier questions pertaining to the basic tags (which are popular on
#their own), I find it unlikely that this new question would reveal any new 
#information. 

### percentage answers upvoted
#Due to the nature of stack overflow, answers can have a negative score, so we will 
#interpret "upvoted" as meaning a non-zero, positive score (indicating helpful)
totalAnswers = nrow(answers)
totalAnswersUpvoted = sum(answers$AnswerScore > 0)
percentUpvoted = as.double(100/totalAnswers) * as.double(totalAnswersUpvoted)
sprintf("The percentage of answers upvoted in this data is %.5f%% of %i total answers.", percentUpvoted, totalAnswers)

### percentage questions with accepted answer
#Since a question may only have 1 user accepted answer, we can simply compare the total 
#number of accepted answers to the total number of questions
totalQuestions = nrow(questions)
totalAcceptedAnswers = sum(answers$IsAcceptedAnswer == TRUE)
percentAccepted = as.double(100/totalQuestions) * as.double(totalAcceptedAnswers)
sprintf("The percentage of answers accepted in this data is %.5f%% of %i total questions.", percentAccepted, totalQuestions)

### average score of accepted answer
acceptedAnswers = filter(answers, IsAcceptedAnswer == TRUE)
avg = sum(acceptedAnswers$AnswerScore) / nrow(acceptedAnswers)
sprintf("The mean score of accepted answers is %.4f.", avg)
#plot variance
filteredAnswers = filter(answers, AnswerScore < 30)
filteredAnswers$IsAcceptedAnswer[filteredAnswers$IsAcceptedAnswer==TRUE] <- "Accepted" #adjust labels
filteredAnswers$IsAcceptedAnswer[filteredAnswers$IsAcceptedAnswer==FALSE]   <- "Not Accepted"
ggplot( data = filteredAnswers, aes(x = AnswerScore)) +
  geom_histogram(binwidth = 1) + 
  facet_wrap(~IsAcceptedAnswer) + 
  labs(title = "Histogram of Answer Score",
       x = "Answer Score",
       y = "Number of Answers") 

### average score of unaccepted answer
unacceptedAnswers = filter(answers, IsAcceptedAnswer == FALSE)
avg = sum(unacceptedAnswers$AnswerScore) / nrow(unacceptedAnswers)
sprintf("The mean score of not accepted answers is %.4f.", avg)

### number of accepted answers with a score of 0 or less
acceptedLowScore = (100/nrow(acceptedAnswers)) * sum(answers$IsAcceptedAnswer == TRUE & answers$AnswerScore <= 0)
sprintf("The percent of accepted answers with a score of 0 or less is %.2f%%.", acceptedLowScore)
#185 accepted answers had a score of less than 0

### do higher answer scores correlate with asker acceptance?
#limit data to only accepted answers for analysis
acceptedAnswers = filter(answers, IsAcceptedAnswer == TRUE)
#Use Pearson's correlation to examine correlation between AnswerScore and IsAcceptedAnswer
cor.test(answers$AnswerScore, as.numeric(answers$IsAcceptedAnswer))
#correlation is 0.0828. So not a very good linear correlation
#(try to account for less popular/visited questions, score should be a percentage out of
# highest score for an answer to that question)?


### do images/code/links being included in answers correlate with asker acceptance?
cor.test(as.numeric(qa$ContainsImage), as.numeric(qa$IsAcceptedAnswer))
#.091
cor.test(as.numeric(qa$ContainsCode), as.numeric(qa$IsAcceptedAnswer))
#.103
cor.test(as.numeric(qa$ContainsLink), as.numeric(qa$IsAcceptedAnswer))
#.015

