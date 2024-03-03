#load packages
library(ngram) #for the wordcount function
library(cld3) #for the detect_language function
library(tm) #for the stripWhitespace function
library(dplyr)
library(koRpus) #for the tokenize and MATTR functions
library(tidytext) #for the unnest_tokens function and getting the Afinn lexicon using get_sentiments("afinn")
library(lexicon) #for getting the hash_nrc_emotions lexicon

#read file
blog<-read.csv("blogtext.csv",stringsAsFactors = F,skipNul = T)

rowTotal<-nrow(blog) #count total rows in data.frame
blog$length<-rep(NA,rowTotal) #create a new column in the dataframe for holding the length values. Each cell has NA as the initial value
blog$language<-rep(NA,rowTotal)

#Cleaning the blog text
for (i in 1:rowTotal){
  text<-tm::stripWhitespace(blog$text[i]) #remove white spaces
  blog$length[i]<-ngram::wordcount(text) #count text length
  blog$language[i]<-cld3::detect_language(text) #detect text language
  blog$text[i]<-text #save the cleaned version of text without white spaces
}
blog<-blog %>%
  filter(length>120) %>% #remove entries with fewer than 120 words
  filter(language=="en") #remove non-English blog entries
rowTotal<-nrow(blog) #update total row count in data.frame after data cleaning

#drop the blog$language column because we don't need it anymore now that all non-English blogs have been removed
blog<-blog %>%
  select(-language)

#calculate MATTR for filtering out weird blog entries. Use the R package koRpus
blog$mattr<-rep(0,rowTotal) #create a new column in the dataframe for holding the mattr values. Each cell has NA as the initial value
install.koRpus.lang("en") #install koRpus English tokeniser
library(koRpus.lang.en) #load koRpus English tokeniser for calculating MATTR
for (i in 1:rowTotal) {
  if (blog$length[i]>120) {
    text<-tokenize(blog$text[i],format="obj",lang="en") #koRpus requires tokenisation before MATTR calculation. Takes at least 30 mins to compute.
    blog$mattr[i]<-MATTR(text)[[1]] #calculate MATTR and save result in the new column
  }
}

#remove blog entries that look unusual
blog<-blog %>%
  filter(mattr>=0.35) %>% #the value is determined through manual inspection of the data
  filter(mattr<0.9)

#sentiment analysis
rowTotal<-nrow(blog) #update total row count in data.frame after data cleaning
blog$blogID<-1:rowTotal #create a new column containing unique blogIDs for joining data.frames later

#arrange blog in bag-of-words format as it is required for sentiment analysis
#sentiment analysis approach 1: Use the Affin lexicon
blogTidy<-blog %>%
  select(blogID,text) %>% #only two columns are needed for sentiment analysis
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("afinn"),by="word") #join blogTidy with Afinn by common column "word"

#sum the polarity by blog and save the results in data.frame blogTidy
blogTidy<-blogTidy %>%
  group_by(blogID) %>%
  summarise(polarity=sum(value)) #save the sum of each blog text in a column called polarity

#left join the polarity column back to the blog data.frame by blogID
blog<-blog %>%
  left_join(blogTidy,by="blogID")

#assign 0 to blog$polarity for blogs contain no words with sentiment
blog$polarity[is.na(blog$polarity)]<-0

#sentiment analysis approach 2: Use NRC emotions lexicon
blogTidy<-blog %>% #we overwrite the old data.frame blogTidy. Otherwise, you can create a new data.frame
  select(blogID,text) %>% #only two columns are needed for sentiment analysis
  unnest_tokens(word,text) %>%
  inner_join(hash_nrc_emotions,by=c("word"="token")) #note that the hash_nrc_emotions has two columns: token and emotion
blogTidy<-blogTidy %>%
  group_by(blogID,emotion) %>%
  summarise(value=n()) #save the sum of each blog text in a column called polarity

#rearrange blogTidy in wide format for merging back to data.frame blog
blogTidy<-blogTidy %>%
  pivot_wider(names_from=emotion,values_from = value)
blogTidy[is.na(blogTidy)]<-0 #Some blogs do not have all eight emotions, so they have NA in their cells. These NAs need to be replaced by 0.

#left join the emotions column back to the blog data.frame by blogID
blog<-blog %>%
  left_join(blogTidy,by="blogID")

#Optional: remove unnecessary variables to avoid confusing yourself
#save the workspace and export the data.frame blog now in case the computer crashes later and you lose everything
write.csv(blog,file="blogCleaned.csv",row.names = FALSE)
rm(blogTidy)
rm(text)
save.image(file='ENGL4026.RData')
