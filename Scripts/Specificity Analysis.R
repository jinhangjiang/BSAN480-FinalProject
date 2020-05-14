tag<- read.csv("tag.csv")
quantile(tag$RELEVANCE, probs = seq(0,1,0.10))

tag.top<-tag[tag$RELEVANCE>=0.2905,]
text<- tag.top$TAG

library(stringr)
library(spacyr)
spacy_initialize(model = 'en_core_web_sm')

document<-spacy_parse(as.character(text))
document_entity<-entity_extract(document,type = "all")
nrow(document_entity)/sum(str_count(unlist(text), '\\w+'))


tag.bottom<-tag[tag$RELEVANCE<=0.012,]
document1<-spacy_parse(as.character(tag.bottom$TAG))
document_entity1<-entity_extract(document1,type = "all")
nrow(document_entity1)/sum(str_count(unlist(tag.bottom$TAG), '\\w+'))

a<-c(0.04143767, 0.2300687)
b<-c(mean(tag.top$RELEVANCE), mean(tag.bottom$RELEVANCE))
cor(a,b,method = "pearson")
plot(a~b, type="l")
