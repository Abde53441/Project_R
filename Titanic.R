options(scipen = 999,stringsAsFactors = F)

library(dplyr)

titanic_train=read.csv("C:/Users/Humza Ali/Documents/Data/titanic_train.csv",stringsAsFactors = F)

titanic_test= read.csv("C:/Users/Humza Ali/Documents/Data/titanic_test.csv",stringsAsFactors = F)

dim(titanic_train)

dim(titanic_test)

setdiff(names(titanic_train),names(titanic_test))

glimpse(titanic_train)

sapply(titanic_train, function(x) sum(is.na(x)))

titanic_train$Age[is.na(titanic_train$Age)]=median(titanic_train$Age,na.rm = TRUE)

sapply(titanic_test, function(x) sum(is.na(x)))

titanic_test$Age[is.na(titanic_test$Age)]=median(titanic_test$Age,na.rm = TRUE)
titanic_test$Fare[is.na(titanic_test$Fare)]=round(mean(titanic_train$Fare,na.rm = TRUE),0)

colSums(titanic_train == "")
colSums(titanic_test == "")

table(titanic_train$Embarked)

table(titanic_train$Cabin)

titanic_test$Survived <- NA

titanic_train$data <- "train"
titanic_test$data <- "test"

titanic_all=rbind(titanic_train,titanic_test)

glimpse(titanic_all)

head(titanic_all)

tail(titanic_all)

table(titanic_all$Embarked)

titanic_all$Embarked[titanic_all$Embarked == ""]= "S"

sapply(titanic_all, function(x) length(unique(x)))


titanic_all = titanic_all %>% 
  select(c(-Name,-PassengerId,-Cabin,-Ticket))

glimpse(titanic_all)

table(titanic_all$Sex)

table(titanic_all$Embarked)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

names(titanic_all)[sapply(titanic_all,function(x) is.character(x))]

cat_cols <- c("Sex","Embarked")

for(cat in cat_cols){
  titanic_all=CreateDummies(titanic_all,cat,100)
}

glimpse(titanic_all)

titanic_train=titanic_all %>% filter(data=="train") %>% select(-data)
titanic_test=titanic_all %>% filter(data=="test") %>% select(-data,-Survived)  

set.seed(2)
s=sample(1:nrow(titanic_train),0.75*nrow(titanic_train))
train_75=titanic_train[s,]
test_25=titanic_train[-s,]

library(car)

for_vif=lm(Survived~.,data=train_75)

sort(vif(for_vif),decreasing = T)[1:3]

log_fit <- glm(Survived~.,data=train_75,family = "binomial")

log_fit <- step(log_fit)

log_fit <- glm(Survived ~ Pclass + Age + SibSp + Sex_male + Embarked_S, data = titanic_train,family = 'binomial')

summary(log_fit)

library(pROC)

val.score1=predict(log_fit,newdata = test_25,type='response')

auc(roc(test_25$Survived,val.score1))

train.score1 = predict(log_fit, newdata = train_75, type="response")

auc(roc(train_75$Survived,train.score1))

train.score = predict(log_fit, newdata = titanic_train, type = "response")

auc(roc(titanic_train$Survived,train.score))


real=titanic_train$Survived

cuttoffs = seq(0.001,0.999,0.001)

length(cuttoffs)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cuttoffs){
  print(paste0('cutoff is: ', cutoff))
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,
                    c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]
View(cutoff_data)

library(ggplot2)

ggplot(cutoff_data,aes(x=cutoff,y=Sp))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)

ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()

cutoff_small <- cutoff_long[cutoff_long$Measure %in% c('Sn','Sp'),]

ggplot(cutoff_small,aes(x=cutoff,y=Value,color=Measure))+geom_line()

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

View(cutoff_data)

train.predicted <- as.numeric(train.score > my_cutoff)

test.predicted <- as.numeric(predict(log_fit, titanic_test) > my_cutoff)

titanic_train$train_predicted <- train.predicted

titanic_test$test_predicted <- test.predicted

library(caret)

confusionMatrix(factor(titanic_train$train_predicted),factor(titanic_train$Survived))



test.predicted <- as.numeric(predict(log_fit, titanic_test) > my_cutoff)

write.csv(test.predicted,"Titanic.csv",row.names = FALSE)

## Random Forest ---------------------------------------------------------------------

library(cvTools)
library(randomForest)



titanic_train$Survived <- as.factor(titanic_train$Survived)
glimpse(titanic_train)

param=list(mtry =c(5,10,15,20,25,35),
           ntree = c(50,100,200,500,700),
           maxnodes = c(5,10,15,20,30,50,100),
           nodesize = c(1,2,5,10))


mycost_auc <- function(Survived,yhat){
  roccurve=pROC::roc(Survived,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=50
my_params=subset_paras(param,num_trials)
my_params

myauc=0

for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,Survived ~., 
             data =titanic_train,
             tuning =params,
             folds = cvFolds(nrow(titanic_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  print(score.this)
  print(myauc)
  
  
  if(score.this>myauc){
    #print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    #print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  #print('DONE')
  # uncomment the line above to keep track of progress 
}


best_params
myauc
ci.rf.final=randomForest(Survived~.,
                         mtry=25,
                         ntree=500,
                         maxnodes=50,
                         nodesize=2,
                         data=titanic_train)


ci.rf.final

test.predicted <- as.numeric(predict(ci.rf.final, titanic_test) > my_cutoff)

write.csv(test.predicted,"Titanic1.csv",row.names = FALSE)



























