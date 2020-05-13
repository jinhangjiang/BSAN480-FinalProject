tag<- read.csv("tag.csv")
movie_new<-read.csv("movie_new.csv")
library(tidyverse)

newtag<-tag[,-2] %>% 
  group_by(tag$MOVIEID) %>%
  summarise_all(list(Mean=mean))

newtag
length(unique(tag$MOVIEID))  #6273
length(unique(newtag$RELEVANCE_Mean)) #6225


#merge data

newdata<-merge(x=movie_new, y=newtag, by=1,all.x = TRUE)


newdata$MOVIEID_Mean<-NULL
length(newdata$RELEVANCE_Mean[newdata$RELEVANCE_Mean=='NA']) #12047

newdata$RELEVANCE_Mean[which(is.na(newdata$RELEVANCE_Mean))] <- 0.0000001
newdata$RELEVANCE_Mean_1<-newdata$RELEVANCE_Mean
newdata$RELEVANCE_Mean_1[newdata$RELEVANCE_Mean==0.0000001] <- mean(newtag$RELEVANCE_Mean)


mean(newdata$RELEVANCE_Mean)
mean(newtag$RELEVANCE_Mean)
mean(newdata$RELEVANCE_Mean_1)
#newdata[newdata$X==3257,]
#newtag[newtag$`tag$MOVIEID`==3257,3]
hist(newdata$RELEVANCE_Mean[newdata$RELEVANCE_Mean>=0.000001])

write.csv(newdata, file = "cleandata.csv")

