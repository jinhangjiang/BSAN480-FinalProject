movie<- read.csv("movie.csv")
movie_new<-movie


#### exclude unnecessary variables & transform extreme values
# extreme values
movie_new$BUDGET[movie_new$BUDGET==0]
movie_new$BUDGET<-log(movie$BUDGET+1)
movie_new$BUDGET[movie_new$BUDGET=="0.00000"]<-mean(movie_new$BUDGET)
movie_new$REVENUElog<-log(movie$REVENUE+1)
movie_new$REVENUElog[movie_new$REVENUElog=="0.00000"]<- mean(movie_new$REVENUElog)



# Useless values
movie_new$MOVIEID<-NULL
movie_new$IMDBID<-NULL  
movie_new$TITLETYPE<-NULL
movie_new$ORIGINAL_LANGUAGE<-NULL
movie_new$GENRES<-NULL
movie_new$RUNTIMEMINUTES<-NULL



# Factor Values
movie_new$DIRECTOR_GENDER<-as.factor(movie_new$DIRECTOR_GENDER)
movie_new$ACTOR1_GENDER<-as.factor(movie_new$ACTOR1_GENDER)
movie_new$ACTOR2_GENDER<-as.factor(movie_new$ACTOR2_GENDER)

col<-colnames(tmd)
movie_new[col]<-lapply(movie_new[col], as.factor)
sapply(movie_new,class)


# transform genres to DTM and append back to the new dataset
library(tm)
genre<-movie$GENRES
genre<-as.data.frame(genre)

genre$genre<-gsub("|"," ", genre$genre, fixed = TRUE)
genre$genre<-gsub("(no genres listed)","",genre$genre, fixed = TRUE)

head(genre)

mycorpus<-Corpus(VectorSource(genre$genre))
tmd<-DocumentTermMatrix(mycorpus)
inspect(tmd)
tmd<-as.matrix(tmd)
tmd<-as.data.frame(tmd)
movie_new<-cbind(movie_new,tmd)
head(movie_new)

no_genre<-which(movie$GENRES=="(no genres listed)")
movie_new$NO_GENRE<-rep(0,nrow(movie_new))
movie_new$NO_GENRE[which(movie$GENRES=="(no genres listed)")]<-1

genres_summary<-matrix(nrow = 20, ncol = 2)
rownames(genres_summary)<-colnames(tmd) 
colnames(genres_summary)<-c('SUM','RANK')
genres_summary<-as.data.frame(genres_summary)

for (i in 1:ncol(tmd)) {
  genres_summary[i,1]<-sum(tmd[,i])
  
}
genres_summary$RANK[order(genres_summary$SUM, decreasing = TRUE)]<-1:nrow(genres_summary)
genres_summary


write.csv(genres_summary, file = "genres_summary.csv")
write.csv(movie_new, file = "movie_new.csv")

tmd$no_genre<- movie_new$NO_GENRE
write.csv(tmd, file = "genre_matrix.csv")
### make categorical variables





