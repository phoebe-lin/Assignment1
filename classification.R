library(dplyr)
library(tm) #for creating a document-term matrix (DTM)
library(lexicon)
library(rpart)
library(rpart.plot) #for plotting

set.seed(50) #to ensure the results are reproducible
data<-read.csv(file.choose(),stringsAsFactors=F)

#Create a DTM, using package tm
corpus<-VCorpus(DataframeSource(data))
corpus<-tm_map(corpus, stripWhitespace) #remove empty spaces
corpus<-tm_map(corpus,removeWords, sw_loughran_mcdonald_long) #remove stopwords. Many stopword lists available through R package lexicon. check for yourself
corpus<-tm_map(corpus, removePunctuation) #remove punctuation
corpus<-tm_map(corpus,removeNumbers) #remove numbers

dtm<-tm::DocumentTermMatrix(corpus) %>%
  removeSparseTerms(0.999) #remove from the DTM terms that appear in isolated docs. They could be typos or peculiar words

#prepare a data.frame for model training
dtm.df<-dtm %>% as.matrix() %>% as.data.frame() %>% #convert DTM to a data.frame. This is a requirement of rpart()
  mutate(genre=data$genre) #add the genre labels to the DTM

#split the data, which has already been converted to DTM, into training/test sets
t<-sample(nrow(dtm.df),7500) #this 7500 is about 60-70% of the total data
train<-dtm.df[t,]
test<-dtm.df[-t,]

#Fit classification model using rpart()
model<-rpart(genre~.,data=train,method = "class")
plotcp(model) #note the corresponding cp of the elbow to guide pruning
model.pruned<-prune(model,cp=.033) #prune model


test$predicted<-predict(model.pruned,test[,-7123], type = c("class"))
table(test$genre,test$predicted) #generate a confusion matrix to assess how good the model is at prediction
rpart.plot(model.pruned, type = 3, fallen.leaves = TRUE)

#Classification tree (rpart) is good, but not as advanced as random forest. Random forest produces more accurate predictions, but also takes longer to train. Well. Good things take time!
#It takes at least an hour to run the following lines. Beware!
library(caret)
rf<-train(genre~.,data=train,method="ranger",tuneLength=3,trControl=trainControl(method="cv",number = 3)) #caret does the train/test split and 10-fold cross validation (cv) for us. No wonder Phoebe recommends the use of R package caret!
plot(rf)
test$predictedrf<-predict(rf,test[,c(1:7122)]) #I exclude column 7123 because it contains the genre labels
table(test$genre,test$predictedrf) #generate a confusion matrix to assess how good the model is at prediction.

