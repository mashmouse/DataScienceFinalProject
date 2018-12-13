#Data cleaning final proj

library(dplyr)
library(tidyr)
library(stringr)
#library(ngram)
library(vecsets)

#upload raw data from where they have been downloaded to
#source (https://www.kaggle.com/pedromiguelmarques/tidytext-analysis-of-r-questions-on-stack-overflow/data?scriptVersionId=408262)
answers = read.csv("~/Downloads/rquestions/Answers.csv")
tags = read.csv("~/Downloads/rquestions/Tags.csv")
questions = read.csv("~/Downloads/rquestions/Questions.csv")


#mutate answer body length
getBodyTextCount = function(content) {
  #strip html code tag and contents
  #strip remaining html
  noHtml = gsub("<.*?>", " ", gsub("<code>(.*?)</code>", "", content))
  #split into list of words
  words = strsplit(noHtml, "\\s+")[[1]]
  #remove empty string
  words = words[words != ""]
  return(length(words))
}
answers = mutate(answers,  answerTextLength = mapply(getBodyTextCount, answers$Body))


answers = mutate(answers, containsImage = grepl("<img src", answers$Body))

#mutate boolean answer body contains code
answers = mutate(answers, containsCode = grepl("<code>(.*?)</code>", answers$Body))

#mutate boolean answer body contains link
answers = mutate(answers, containsLink = grepl("<a href", answers$Body))

#alter similar column names between questions and answers to prepare for merge
answers$Id = NULL #destroy useless column

questions = questions[,1:6]
questions$Id = as.numeric(questions$Id)

answers = answers[,2:11]
names(answers) = c("AnswerOwnerUserId", "AnswerCreationDate", "ParentId", "AnswerScore", "IsAcceptedAnswer", "AnswerBody", "AnswerBodyTextLength", "ContainsImage", "ContainsCode", "ContainsLink")
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

write.csv(qa, file = "RquestionsData.csv")

#number of intersecting noun phrases between quetion and answer
#getIntersectionCount = function(qBody, aBody) {
  #strip html and puncutation from qBody and aBody and lower text
  #qText = tolower(gsub("[[:punct:]]", "", gsub("<.*?>", " ", qBody)))
  #aText = tolower(gsub("[[:punct:]]", "", gsub("<.*?>", " ", aBody)))
  #do part of speech tagging on stripped bodies.
  #AwordAnnotation <- annotate(aText, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  #QwordAnnotation <- annotate(qText, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  #APOSAnnotation <- annotate(aText, Maxent_POS_Tag_Annotator(), AwordAnnotation)
  #QPOSAnnotation <- annotate(qText, Maxent_POS_Tag_Annotator(), QwordAnnotation)
  #APOSwords <- subset(APOSAnnotation, type == "word")
  #QPOSwords <- subset(QPOSAnnotation, type == "word")
  # get tags for words in body
  Atags <- sapply(APOSwords$features, '[[', "POS")
  Qtags <- sapply(QPOSwords$features, '[[', "POS")
  #get words vec split on white space   
  aWords = strsplit(aText, "\\s+")[[1]]
  qWords = strsplit(qText, "\\s+")[[1]]
  #(eliminate empty string entries)
  aWords = aWords[aWords != ""]
  qWords = qWords[qWords != ""]
  # join the vectors of words into data frame. Filter to keep only nouns
  Atagged <- data.frame(Tokens = unlist(aWords, use.names = FALSE), Tags = Atags)
  Atagged = filter(Atagged, Tags == "NN" | Tags == "NNS" | Tags == "NNPS" | Tags == "NNP" ) 
  Qtagged <- data.frame(Tokens = unlist(qWords, use.names = FALSE), Tags = Qtags)
  Qtagged = filter(Qtagged, Tags == "NN" | Tags == "NNS" | Tags == "NNPS" | Tags == "NNP" ) 
  #get set intersection of words that were tagged as nouns 
  #intersection = intersect(Atagged$Tokens, Qtagged$Tokens)
  #return number of intersections
  #return(length(intersection))
#}
checkWord <- function(word){
  exclusions <- c("I", "you", "he", "she",
                  "it", "we", "they", "me", "him",
                  "her", "us", "them", "what", "who",
                  "whom", "mine", "yours", "his", "hers",
                  "ours", "theirs", "this", "that", "these",
                  "those", "whose", "which", "whatever", "whoever",
                  "whomever", "whichever", "myself", "yourself", "himself",
                  "herself", "itself", "ourselves", "themselves","everybody",
                  "anybody", "anyone", "everyone", "nobody", "others",
                  "somebody", "someone", "the", "a", "an")
  for(item in exclusions){
    if(word == item){
      return("")
    }
  }
  return(word)
}

getIntersectionCount<- function(qBody, aBody){
  #to see whether the answer looks like the question
  #strip html and puncutation from qBody and aBody and lower text
  qText = tolower(gsub("[[:punct:]]", " ", gsub("<.*?>", " ", qBody)))
  #print(qText)
  aText = tolower(gsub("[[:punct:]]", " ", gsub("<.*?>", " ", aBody)))
  #print(aText)
  aWords = unlist(strsplit(paste(" ", aText), "\\s+"))
  aWords = mapply(checkWord, aWords)
  aWords = aWords[aWords != ""]
  #print(aWords)
  qWords = unlist(strsplit(paste(" ", qText), "\\s+"))
  qWords = mapply(checkWord, qWords)
  qWords = qWords[qWords != ""]
  #print(qWords)
  #base = legnth(qWords) + length(aWords)
  #for(aWord in aWords){
  #  for(i in 1:length(count)){
  #    if(aWord){
  #      
  #    }
  #  }
  #}
  #inter <- intersect(qWords, aWords) 
  intersec <- vecsets::vintersect(aWords, qWords)
  #print(intersec)
  return(length(intersec)/ (length(qWords) + length(aWords)))
}

qa = mutate(qa, QAwordIntersection = as.numeric(mapply(getIntersectionCount, as.character(qa$QuestionBody), 
                                                     as.character(qa$AnswerBody))))

qa = mutate(qa, ContainsCode = as.integer(as.logical(qa$ContainsCode)))
qa = mutate(qa, ContainsImage = as.integer(as.logical(qa$ContainsImage)))
qa = mutate(qa, ContainsLink = as.integer(as.logical(qa$ContainsLink)))

qa$IsAcceptedAnswer <- factor(ifelse(qa$IsAcceptedAnswer,"True","False"))

write.csv(qa, "cleanRquestionsData.csv")
