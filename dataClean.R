#Data cleaning final proj

library(dplyr)
library(tidyr)
library(stringr)
library(ngram)
#library(openNLP)
#library(NLP)

answers = read.csv("~/DataScience/FinalProject/rquestions/Answers.csv")
tags = read.csv("~/DataScience/FinalProject/rquestions/Tags.csv")
questions = read.csv("~/DataScience/FinalProject/rquestions/Questions.csv")


#mutate answer body length
getBodyTextCount = function(content) {
  #strip html code tag and contents
  #strip remaining html
  return(ngram::wordcount(gsub("<.*?>", " ", gsub("<code>(.*?)</code>", "", content))))
}
answers = mutate(answers,  answerTextLength = mapply(getBodyTextCount, answers$Body))

write.csv(answers, file = "answers_getBodyTextCount.csv")

#umtate boolena answer body contains image
answers = mutate(answers, containsImage = grepl("<img src", answers$Body))

#mutate boolean answer body contains code
answers = mutate(answers, containsCode = grepl("<code>(.*?)</code>", answers$Body))

#mutate boolean answer body contains link
answers = mutate(answers, containsLink = grepl("<a href", answers$Body))

#alter similar column names between questions and answers to prepare for merge
answers$Id = NULL #destroy useless column
names(answers) = c("AnswerOwnerUserId", "AnswerCreationDate", 
                   "ParentId", "AnswerScore", "IsAcceptedAnswer", "AnswerBody")
names(questions) = c("Id", "QuestionOwnerUserId", "QuestionCreationDate", 
                    "QuestionScore", "QuestionTitle", "QuestionBody")

#merge all by id-parentId
qa = inner_join(questions, answers, by = c("Id" = "ParentId"))

#timestamp difference (for each answer) between answer and question in hours
getTimeDif = function(qTime, aTime) {
  #get data from string
  Qdatetime = strsplit(qTime, "T")
  Qdate = Qdatetime[[1]]
  Adatetime = strsplit(aTime, "T")
  Adate = Adatetime[[1]] 
  
  #return time difference in hours
  daysDif = as.numeric(difftime(strptime(Adate[1], format = "%Y-%m-%d"),
                    strptime(Qdate[1], format = "%Y-%m-%d"),units="days"))
  if(daysDif == 0) {
    #find difference in hours on same day
    Qtime = strsplit(Qdate[2], ":")[[1]] # h m s
    Atime = strsplit(Adate[2], ":")[[1]]

    #get time as fractions of hour
    hours = as.numeric(Atime[1]) - as.numeric(Qtime[1])
    mins = (as.numeric(Atime[2]) - as.numeric(Qtime[2])) / 60
    secs = (as.numeric(strsplit(Atime[3], "Z")[[1]][1]) - 
              as.numeric(strsplit(Qtime[3], "Z")[[1]][1])) / 3600
    return(hours + mins + secs)
  } else {
    return(daysDif * 24)
  }
}
options(scipen = 999) #stop scientific notation
qa = mutate(qa, ResponseTimeHours = as.double(mapply(getTimeDif, as.character(qa$QuestionCreationDate), 
                                           as.character(qa$AnswerCreationDate))))

#number of intersecting noun phrases between quetion and answer
getIntersectionCount = function(qBody, aBody) {
  #strip html and puncutation from qBody and aBody and lower text
  qText = tolower(gsub("[[:punct:]]", "", gsub("<.*?>", " ", qBody)))
  aText = tolower(gsub("[[:punct:]]", "", gsub("<.*?>", " ", aBody)))
  #do part of speech tagging on stripped bodies.
  AwordAnnotation <- annotate(aText, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  QwordAnnotation <- annotate(qText, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  APOSAnnotation <- annotate(aText, Maxent_POS_Tag_Annotator(), AwordAnnotation)
  QPOSAnnotation <- annotate(qText, Maxent_POS_Tag_Annotator(), QwordAnnotation)
  APOSwords <- subset(APOSAnnotation, type == "word")
  QPOSwords <- subset(QPOSAnnotation, type == "word")
  # get tags for words in body
  Atags <- sapply(APOSwords$features, '[[', "POS")
  Qtags <- sapply(QPOSwords$features, '[[', "POS")
  # join the vectors of words into data frame. Filter to keep only nouns
  aWords = strsplit(aText, "\\s+")
  qWords = strsplit(qText, "\\s+")
  Atagged <- data.frame(Tokens = unlist(aWords, use.names = FALSE), Tags = Atags)
  Atagged = filter(Atagged, Tags == "NN" | Tags == "NNS" | Tags == "NNPS" | Tags == "NNP" ) 
  Qtagged <- data.frame(Tokens = unlist(qWords, use.names = FALSE), Tags = Qtags)
  Qtagged = filter(Qtagged, Tags == "NN" | Tags == "NNS" | Tags == "NNPS" | Tags == "NNP" ) 
  #get set intersection of words that were tagged as nouns 
  intersection = intersect(Atagged$Tokens, Qtagged$Tokens)
  #return number of intersections
  return(length(intersection))
}
qa = mutate(qa, QAwordIntersection = as.numeric(mapply(getIntersectionCount, as.character(qa$QuestionBody), 
                                                     as.character(qa$AnswerBody))))
