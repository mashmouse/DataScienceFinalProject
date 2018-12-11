# Statistical Data Analysis

#Introductory Analysis of data
#  - What percentage of all answers are upvoted? 
#  - What percentage of all questions have (asker accepted) answers?
#  - What are the most popular tags on questions with accepted answers?
#  - Answers of which tag are the most successful? The least successful?
#  - Do higher answer scores and answer acceptance correlate?

# raw data (for simplicity with early questions)
answers = read.csv("~/Downloads/rquestions/answers.csv")
questions = read.csv("~/Downloads/rquestions/questions.csv")
# upload the cleaned csv R questions data
qa = read.csv("~/Desktop/Data\ Science/DataScienceFinalProject/cleanData.csv")


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

### do higher answer scores correlate with asker acceptance
#create a model to examine correlation between these variables
#limit data to only accepted answers for analysis
#(but to account for less popular/visited questions, score should be a percentage out of
# highest score for an answer to that question)
