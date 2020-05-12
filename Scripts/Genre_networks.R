genres<- read.csv("tr_embedding.csv")
genres
library(fpc)


## optimal k
wss<- NULL
for (i in 1:10){
  fit1=kmeans(genres[,-1],centers = i)
  wss=c(wss, fit1$tot.withinss)
}

plot(1:10, wss, type = "o", main = "WSS k=10")

fit=kmeans(genres[,-1],5)
plotcluster(genres[,-1], fit$cluster)


#subsample
subsample <- list()

for(i in 1:5){
  subsample[[i]]<- genres[fit$cluster==i,]
}
subsample[[1]]
subsample[[2]]
subsample[[3]]
subsample[[4]]
subsample[[5]]
