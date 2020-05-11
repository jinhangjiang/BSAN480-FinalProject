genres<- read.csv("embedding.csv")
genres
library(fpc)


## optimal k
wss<- NULL
for (i in 1:10){
  fit1=kmeans(genres[,-1],centers = i)
  wss=c(wss, fit1$tot.withinss)
}

plot(1:10, wss, type = "o")

fit=kmeans(genres[,-1],4)
plotcluster(genres[,-1], fit$cluster)


#subsample
subsample <- list()

for(i in 1:4){
  subsample[[i]]<- genres[fit$cluster==i,]
}
subsample[[4]]

