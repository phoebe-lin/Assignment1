#load necessary packages
library(dplyr)
library(ggplot2)

#check distribution of data
blog %>%
  group_by(gender) %>%
  summarise(count=n())
blog %>%
  group_by(topic) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=topic,y=count)) + geom_boxplot()
blog %>%
  group_by(sign) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=sign,y=count)) + geom_boxplot()
blog %>%
  group_by(age) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=age,y=count)) + geom_smooth(se=FALSE)

#run linear regression
#What explains blog length?
lm1<-lm(data=blog,length~gender+age+topic+sign+mattr+polarity) #don't add date and ID because they don't make sense!
lm2<-lm(data=blog,length~gender+age+sign+mattr+polarity) #try dropping sign as a predictor
summary(lm1)
summary(lm2)
anova(lm2,lm1) #remember to put the simpler model first, the more complex model second
#if anova comes back with a significant p-value, the addition of an extra predictor in the more complex model is justified

#now that all factors are significant. Start interepting the model and plot graphs to illutrate the relationship. Although you should note that the adjusted R-squared is very low.
#remember to add graph title and appropriate axis names
#can you work out when you should use geom_boxplot() or a geom_smooth()?
ggplot(aes(x=gender,y=length),data=blog) + geom_boxplot() #there seem to be many outliers in the boxplot. Maybe worth deleting the outliers and re-run the analysis
ggplot(aes(x=age,y=length),data=blog) + geom_smooth(se=FALSE)
ggplot(aes(x=mattr,y=length),data=blog) + geom_smooth(se=FALSE)
ggplot(aes(x=polarity,y=length),data=blog[!blog$polarity==0,]) + geom_smooth(se=FALSE) #the polarity graph looks very wrong. We will have to clean the data further and try other analyses. We will only cover linear regression in this course. You can read up if you want to try any advanced analyses.
#that the plots do not look linear show you that linear regression may not be the right method, or you may fit non-linear terms. But this is outside of the scope of this intro course.

#Now you can try to running linear regrssion on mattr, polarity


#let's try to explore which signs are similar in terms of sentiments using hierarchical clustering
blogSentiment<-blog %>%
  select(length,sign,anger:trust)
blogSentiment$anger<-blogSentiment$anger/blogSentiment$length
blogSentiment$anticipation<-blogSentiment$anticipation/blogSentiment$length
blogSentiment$disgust<-blogSentiment$disgust/blogSentiment$length
blogSentiment$fear<-blogSentiment$fear/blogSentiment$length
blogSentiment$joy<-blogSentiment$joy/blogSentiment$length
blogSentiment$sadness<-blogSentiment$sadness/blogSentiment$length
blogSentiment$surprise<-blogSentiment$surprise/blogSentiment$length
blogSentiment$trust<-blogSentiment$trust/blogSentiment$length
blogSentiment[is.na(blogSentiment)]<-0
blogSentiment<-blogSentiment %>%
  group_by(sign) %>%
  summarise(anger=mean(anger),anticipation=mean(anticipation,na.rm = T),
            disgust=mean(disgust,na.rm = T),fear=mean(fear,na.rm = T),
            joy=mean(joy,na.rm = T),sadness=mean(sadness,na.rm = T),
            surprise=mean(surprise,na.rm = T),trust=mean(trust,na.rm = T))

#hierarchical clustering takes a matrix, so we need to convert blogSentiment from data.frame to matrix
names<-blogSentiment$sign
blogSentiment<-blogSentiment[,-1] #remove the first column because hierarchical clustering requires the matrix to contain numerics only
blogSentiment<-as.matrix(blogSentiment)
row.names(blogSentiment)<-names
plot(hclust(dist(blogSentiment))) #plot the result of hierarchical clustering as a dendrogram
