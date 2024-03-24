install.packages("topicmodels")
library(topicmodels) #for fitting an LDA
library(ldatuning) #for determining the number of topics in the data, aided by the plot. Use functions: FindTopicsNumber() and FindTopicsNumber_plot() 
library(forcats) #for the fct_reorder() function
set.seed(50)

#remove rows with rowSum<1
result <- ldatuning::FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  verbose = TRUE
)
ldatuning::FindTopicsNumber_plot(result)

ldaModel<-LDA(dtm,k=18,method="Gibbs") # The number of topics (k) is set to 18 because of the plot
tidyLDA<-ldaModel %>% tidy("beta")

#Find the top key terms
wordProbs<-tidyLDA %>%
  group_by(topic) %>%
  slice_max(beta,n=15) %>%
  ungroup() %>%
  mutate(term2=fct_reorder(term,beta))

#Visualise the results as barplots
ggplot(wordProbs,aes(term2,beta,fill=as.factor(topic)))+
  geom_col(show.legend=F)+
  facet_wrap(~topic,scales = "free")+
  coord_flip()

#Visualise the results as a word cloud
library(wordcloud)
library(viridisLite)
cloudPlot<-tidyLDA %>% filter(topic==2) %>% arrange(desc(beta))
wordcloud(cloudPlot$term,cloudPlot$beta,c(15,1),max.words=80,colors = cividis(n = 10))


