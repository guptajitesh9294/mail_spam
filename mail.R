sms_data <- read.csv(file.choose())
View(sms_data)
attach(sms_data)
class(sms_data)
str(sms_data)

#sms_data$type <- as.factor(sms_data$type)
sms_data$text<- as.character(sms_data$text)
str(sms_data)
as.character(sms_data$text)



library(tm)
# Prepare corpuse for the text data 

sms_corpous <- Corpus(VectorSource(sms_data$text))
sms_corpous
class(sms_corpous)
sms_corpous <- tm_map(sms_corpous,function(x) iconv(enc2utf8(x),sub = 'byte'))

# Cleaning data (removing unwanted symbols)
corpus_clean <- tm_map(sms_corpous,toupper)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean,removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean,removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)
# Do not run the plainTextDocument
# corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
as.character(corpus_clean)
# create a document-term sparse matrix
#corpus_clean<-Corpus(VectorSource(corpus_clean))
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm
str(sms_dtm)

# if code at 25 shows any error run the code at line 24 first and proceed
as.character(sms_dtm)
# creating training and test datasets
sms_raw_train <- sms_data[1:4169, ]
sms_raw_test  <- sms_data[4170:5558, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5558, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5558]

# check that the proportion of spam is similar
3605/4169

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
dim(sms_dtm_test)
# indicator features for frequent words
sms_dict <- findFreqTerms(sms_dtm_train,5)
sms_dict




sms_train1 <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test1  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
#sms_dict

inspect(sms_corpus_train[1:10])
(sms_dict)
# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

convert_counts




convert_counts


# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train1, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test1, MARGIN = 2, convert_counts)
View(sms_train)
View(sms_test)
dim(sms_train)
##  Training a model on the data ----
library(e1071)
sms_classifier1 <- naiveBayes(sms_raw_train, sms_raw_train$type)
sms_classifier1
View(sms_raw_train)
sms_classifier1
pred <- predict(sms_classifier1, newdata = sms_raw_test)
pred
##  Evaluating model performance ----

spam1 <- spam1[1:10,1]
spam1 <- as.data.frame(spam1)
pred2 <- predict(sms_classifier1, newdata = spam1)
colnames <- c(text)
str(data)

#PV <- as.data.frame(sms_test_pred1)
#xy <- cbind(PV,sms_raw_test$type)
library(gmodels)

confusion <- CrossTable(pred, sms_raw_test$type)
table(sms_raw_test$type) 
c<-as.data.frame(c)
c
1200/1390
# Accuracy 
(pred == sms_raw_test$type)
x<- as.data.frame(sms_test_pred1)

y<- as.data.frame(sms_raw_test$type)
xy<-cbind(xy,z)
z<-as.data.frame(sms_test_pred1 == sms_raw_test$type)



