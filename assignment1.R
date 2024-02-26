install.packages("koRpus")
library(dplyr)
library(tidyr)
library(ggplot2)
library(tm)
library(stringr)
library(tidytext)
library(koRpus)
library(ngram)
library(stringi)

blog<-read.csv("blogtext.csv",stringsAsFactors = F,skipNul = T)
blog$text<-tm::stripWhitespace(blog$text)
#blog$length<-str_count(blog$text,"\\w+")
install.packages("cld3")
library(cld3)
blog$lang<-detect_language(blog$text)

blog2<-blog %>% filter(length>=120) %>% filter(lang=="en") %>% filter(!is.na(mattr))

install.koRpus.lang("en")
library(koRpus.lang.en)

blog$mattr<-rep(NA,nrow(blog))
for (i in i:nrow(blog)) {
  length<-wordcount(blog$text[i])
  blog$length[i]<-length
    if (length>=120) {
      test<- tokenize(blog$text[i],format="obj",lang="en")
      blog$mattr[i]<-MATTR(test)[[1]]
    }
}



summary(blog$length)

blog<-blog %>% arrange(desc(length))
system("say Phoebe")

dim(blog)
blog %>% group_by(topic,gender) %>% summarise(count=n()) %>% arrange(desc(count)) %>% pivot_wider(names_from=gender,values_from=count) %>% mutate(total=female+male) %>% arrange(desc(total))
blog %>% group_by(topic,sign) %>% summarise(count=n()) %>% arrange(desc(count)) %>% pivot_wider(names_from=sign,values_from=count)

stdBlog<-blog %>% filter(topic=="Student")

stdBlog %>% group_by(sign,gender) %>% summarise(count=n(),meanLength=mean(length,na.rm=TRUE)) %>% arrange(desc(count))%>% pivot_wider(names_from=gender,values_from=count) %>% mutate(total=female+male) %>% arrange(desc(total))
sum(stdBlog$length)
stdBlog %>% group_by(sign,gender) %>% summarise(meanLength=mean(length,na.rm=TRUE)) %>% pivot_wider(names_from = gender,values_from = meanLength)
stdBlog %>% group_by(sign) %>% summarise(meanLength=mean(length))

  stdBlog %>% group_by(sign) %>% summarise(q1=quantile(length,0.25),q3=quantile(length,0.75),iqr=IQR(length),lower=q1-iqr*1.5, upper=q3+1.5*iqr)
  stdBlog %>% summarise(q1=quantile(length,0.25),q3=quantile(length,0.75),iqr=IQR(length),lower=q1-iqr*1.5, upper=q3+1.5*iqr)
  
  stdBlog %>% filter(length<619) %>% ggplot(aes(x=sign,y=length))+geom_boxplot()
  ha<-stdBlog %>% filter(length<619)
  summary(aov(length~sign,ha))






#stdBlog$blogId<-row.names(stdBlog)
#stdBlogtidy<-stdBlog %>% unnest_tokens(word,text)
#stdBlogtidy1<-stdBlogtidy %>% inner_join(get_sentiments("afinn"),"word")
#stdBlogtidy1<-stdBlogtidy1 %>% group_by(blogId) %>% summarise(polarity=sum(value))
#stdBlog<-left_join(stdBlog,stdBlogtidy1,by="blogId")
#stdBlog$polarity[is.na(stdBlog$polarity)]<-0

stdBlog<-stdBlog %>% filter(mattr>=0.35) %>% filter(mattr<0.9)
stdBlog<-stdBlog[,c(1:9,12)]
summary(lm(polarity~age+gender+sign+mattr+length,stdBlog))
summary(lm(mattr~age+gender+sign+polarity+length,stdBlog))
summary(lm(length~age+gender+sign+polarity+mattr,stdBlog))


#get rid of blogs with low ttr?

  library(udpipe)
  m_eng <- udpipe_download_model(language = "english-ewt")
  m_eng <- udpipe_load_model(file ="english-ewt-ud-2.5-191206.udpipe")
  textdf <- udpipe_annotate(m_eng, x = stdBlog$text) %>% as.data.frame()

summary(lm(mattr~age+gender,blog))
summary(lm(mattr~age+gender+sign+topic,blog))
summary(lm(mattr~age,blog))
ggplot(blog,aes(x=age,y=mattr))+geom_smooth()
ggplot(blog,aes(x=gender,y=mattr))+geom_boxplot()
ggplot(blog,aes(x=lang,y=mattr))+geom_boxplot()
ggplot(blog,aes(x=sign,y=mattr))+geom_boxplot() #too many categories
ggplot(blog,aes(x=topic,y=mattr))+geom_boxplot() #too many categories

blogTidy<-blog %>% unnest_tokens(word,text) %>% inner_join(get_sentiments("afinn"),"word") %>% group_by(blogId) %>% summarise(polarity=sum(value))
blog<-left_join(blog,blogTidy,by="blogId")
#blog$polarity[is.na(blog$polarity)]<-0 咁做可能唔啱
summary(lm(polarity~age+gender+sign+mattr+length,blog))
summary(lm(mattr~age+gender+sign+polarity+length,blog))
summary(lm(length~age+gender+sign+polarity+mattr,blog))

stdBlogtidy<-stdBlog %>% unnest_tokens(word,text) %>% inner_join(hash_nrc_emotions,c("word"="token")) %>% group_by(blogId,emotion) %>% summarise(value=n())
stdBlogTidy2<-stdBlogTidy %>% group_by(blogId) %>% summarise(total=sum(value))
stdBlogTidy<-left_join(stdBlogTidy,stdBlogTidy2,by="blogId")
stdBlogTidy$prop<-stdBlogTidy$value/stdBlogTidy$total
stdBlogTidy<-stdBlogTidy[,c(1:2,5)]
stdBlogTidy %>% pivot_wider(names_from = emotion,values_from = prop)



stdBlogtidy<-stdBlog %>% inner_join(hash_nrc_emotions,"token")
stdBlogtidy<-inner_join(stdBlogtidy,nrc,by="token")

summary(lm(anger~age+gender+sign+topic+length+mattr,blog))
summary(lm(anticipation~age+gender+sign+topic+length+mattr,blog))
summary(lm(disgust~age+gender+sign+topic+length+mattr,blog))
summary(lm(fear~age+gender+sign+topic+length+mattr,blog))
summary(lm(joy~age+gender+sign+topic+length+mattr,blog))
summary(lm(sadness~age+gender+sign+topic+length+mattr,blog))
summary(lm(surprise~age+gender+sign+topic+length+mattr,blog))
summary(lm(trust~age+gender+sign+topic+length+mattr,blog))

library(udpipe)
m_eng <- udpipe_download_model(language = "english-ewt")
m_eng <- udpipe_load_model(file ="english-ewt-ud-2.5-191206.udpipe")

textdf <- udpipe_annotate(m_eng, x = blog$text,doc_id = blog$blogId,tagger = "default", parser = "none") %>% as.data.frame() %>% group_by(doc_id,upos) %>% summarise(count=n())


system("say Phoebe")
