movie<-read.csv("cleandata.csv")


a<-movie


a[,c("TITLE","RELEASE_DATE_TXT","PRODUCTION_COMPANIES",
     "DIRECTOR_NAME","DIRECTOR_GENDER","ACTOR1_NAME",
     "ACTOR1_GENDER","ACTOR2_NAME","ACTOR2_GENDER"
     )]<-list(NULL)
write.csv(a, file = "modeldata.csv")



### average rating prediction
a1<-a

a1[,c("BUDGET","REVENUE","VOTE_COUNT","VOTE_AVERAGE",
      "ML_RCOUNT","ML_RATING","PERFORMANCE", "RELEVANCE_MEAN")]<-list(NULL)
a1<-na.omit(a1)
set.seed(1)
index<-sample(nrow(a1), 0.8*nrow(a1))
a1.train<-a1[index,-1]
a1.test<-a1[-index,-1]



null<-lm(a1.train$AVERAGERATING~1, data = a1.train)
full<-lm(a1.train$AVERAGERATING~., data = a1.train)
step(null, list(lower = null, upper = full), direction = c("both"))

fit<-lm(formula = a1.train$AVERAGERATING ~ ML_RCOUNTlog + STARTYEAR + 
     documentary + drama + horror + RELEVANCE_Mean_1 + sci.fi + 
     action + RUNTIME + NO_GENRE + NUMVOTESlog + Actor1_Sex + 
     budget_log + animation + REVENUElog + western + 
     adventure + film.noir + children + mystery + fantasy, data = a1.train)
summary(fit)

plot(fit$res~fit$fitted)
hist(fit$res)
AIC(fit)

pred1<-predict(fit, newdata = a1.test)
MSPE1<-mean((a1.test$AVERAGERATING-pred1)^2)
MSPE1

#### use performance to predict
a2<-a
a2[,c("BUDGET","REVENUE","VOTE_COUNT","VOTE_AVERAGE",
      "ML_RCOUNT","ML_RATING","AVERAGERATING", "RELEVANCE_MEAN")]<-list(NULL)
a2<-na.omit(a2)
set.seed(2)
index<-sample(nrow(a1), 0.8*nrow(a1))
a2.train<-a2[index,-1]
a2.test<-a2[-index,-1]

null<-glm(a2.train$PERFORMANCE~1, data = a2.train, family = binomial)
full<-glm(a2.train$PERFORMANCE~., data = a2.train, family = binomial)
step(null, list(lower = null, upper = full), direction = c("both"))
fit2<-glm(formula = a2.train$PERFORMANCE ~ RELEVANCE_Mean_1 + documentary + 
            ML_RCOUNTlog + STARTYEAR + drama + sci.fi + Actor1_Sex + 
            RUNTIME + action + horror + NUMVOTESlog + REVENUElog + NO_GENRE + 
            budget_log + film.noir + thriller + 
            animation + adventure + romance + crime, family = binomial, data = a2.train)
summary(fit2)
hist(fit2$res)

library(ROCR)
pred2<-prediction(predict(fit2, newdata = a2.test, type = "response"), a2.test$PERFORMANCE)
perf2<- performance(pred2, "tpr","fpr")
plot(perf2, colorize = TRUE, main = "ROC curve for model 2")
unlist(slot(performance(pred2, "auc"),"y.values"))


#get the MR
pcut<-mean(a2$PERFORMANCE)
pred2.test<-predict(fit2, newdata = a2.test, type = "response")
class1.test<-(pred2.test>pcut)*1
confmat<-table(a2.test$PERFORMANCE, class1.test, dnn = c("TRUE","PRED"))

#get mr
MR<-mean(class1.test!=a2.test$PERFORMANCE)
MR

# FNR 
FNR<- prop.table(confmat)[2,1]
FNR

# FPR
FPR<- prop.table(confmat)[1,2]
FPR


## optimal cutoff
cost<- function(pcut, weightFP,weightFN, true01, pred.prob){
  class<-(pred.prob>pcut)*1
  FP<- sum(class==1 & true01==0)
  FN<- sum(class==0 & true01==1)
  totalcost<- weightFP*FP+weightFN*FN
  return(totalcost)
}

pcut.seq<- seq(0.1, 0.99, by=0.1)
totcost <- NULL
for (i in 1:length(pcut.seq)) {
  totcost[i]<-cost(pcut = pcut.seq[i], weightFP = 1, weightFN = 1, true01 = a2.test$PERFORMANCE,
                   pred.prob = class1.test)
}
pcut.seq[which.min(totcost)] #0.001

pcut<-0.2
pred2.test<- predict(fit2, newdata = a2.test, type = "response")
class2.test<-(pred2.test>pcut)*1
confmat2<-table(a2.test$PERFORMANCE, class2.test, dnn = c("True","Pred"))
confmat2


#get mr
MR<-mean(class2.test!=a2.test$PERFORMANCE)
MR

# FNR 
FNR<- prop.table(confmat2)[2,1]
FNR

# FPR
FPR<- prop.table(confmat2)[1,2]
FPR
a2[,c("RELEVANCE_Mean")]<-NULL
a2<-na.omit(a2)
library(boot)
auc<-function(obs, pred){
  pred <- prediction(pred, obs)
  unlist(slot(performance(pred, "auc"), "y.values"))
}
fit3<-glm(formula = a2$PERFORMANCE ~ RELEVANCE_Mean_1 + documentary + 
            ML_RCOUNTlog + STARTYEAR + drama + sci.fi + Actor1_Sex + 
            RUNTIME + action + horror + NUMVOTESlog + REVENUElog + NO_GENRE + 
            budget_log + film.noir + thriller + 
            animation + adventure + romance + crime -1, family = binomial, data = a2)

cv1<-cv.glm(data = a2, glmfit = fit3, cost = auc, K=10)
cv1$delta[2]
