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
