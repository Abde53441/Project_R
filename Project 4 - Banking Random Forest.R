options(scipen = 999,stringsAsFactors = F)

library(dplyr)

bank_train=read.csv("C:/Users/Humza Ali/Documents/Data/Project 4 - Banking/bank-full_train.csv",stringsAsFactors = F)

bank_test= read.csv("C:/Users/Humza Ali/Documents/Data/Project 4 - Banking/bank-full_test.csv",stringsAsFactors = F)


dim(bank_train)
dim(bank_test)

head(bank_train)
head(bank_test)

glimpse(bank_train)

setdiff(names(bank_train),names(bank_test))

bank_test$y <- NA

bank_train$data <- "train"
bank_test$data <- "test"

bank_all <- rbind(bank_train,bank_test)

head(bank_all)
tail(bank_all)

glimpse(bank_all)

table(bank_all$y)

bank_all$y=ifelse(bank_all$y=="yes",1,0)

bank_all$y=as.numeric(bank_all$y)

table(bank_all$y)

table(bank_all$default)
bank_all$default <- ifelse(bank_all$default=="yes",1,0)
bank_all$default <- as.numeric(bank_all$default)
table(bank_all$default)

table(bank_all$housing)
bank_all$housing <- ifelse(bank_all$housing=="yes",1,0)
bank_all$housing <- as.numeric(bank_all$housing)
table(bank_all$housing)

table(bank_all$loan)
bank_all$loan <- ifelse(bank_all$loan=="yes",1,0)
bank_all$loan <- as.numeric(bank_all$loan)
table(bank_all$loan)

glimpse(bank_all)


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

names(bank_all)[sapply(bank_all,function(x) is.character(x))]

cat_cols <- c("job","marital","education","contact","month","poutcome")

for(cat in cat_cols){
  bank_all=CreateDummies(bank_all,cat,700)
}

glimpse(bank_all)

names(bank_all)[colSums(is.na(bank_all))>0]

bank_train=bank_all %>% filter(data=="train") %>% select(-data)
bank_test=bank_all %>% filter(data=="test") %>% select(-data,-y)  

set.seed(2)
s=sample(1:nrow(bank_train),0.8*nrow(bank_train))
train_80=bank_train[s,]
test_20=bank_train[-s,]

library(car)

for_vif=lm(y~.-ID,data=train_80)

sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(y~.-ID - job_management - poutcome_unknown -education_secondary -month_may -contact_unknown,data=train_80)

sort(vif(for_vif),decreasing = T)[1:3]

log_fit <- glm(y~.-ID - job_management - poutcome_unknown -education_secondary -month_may -contact_unknown,data=train_80,family = "binomial")

log_fit <- step(log_fit)

log_fit=glm(y ~ age + balance + housing + loan + day + duration + 
              campaign + pdays + previous + job_housemaid + job_unemployed +
              job_self_employed + job_retired + job_services + job_technician + 
              job_blue_collar + marital_single + education_primary + contact_cellular + 
              month_jan + month_feb + month_nov + month_aug + month_jul + 
              poutcome_other + poutcome_failure,data=bank_train,family='binomial')

summary(log_fit)

library(pROC)
library(cvTools)

val.score=predict(log_fit,newdata = test_20,type='response')

auc(roc(test_20$y,val.score))

train.score1 = predict(log_fit, newdata = train_80, type="response")

auc(roc(train_80$y,train.score1))

train.score = predict(log_fit, newdata = bank_train, type = "response")

real=bank_train$y

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

library(randomForest)

## Random Forest

bank_train$y <- as.factor(bank_train$y)
glimpse(bank_train)

param=list(mtry =c(5,10,15,20,25,35),
           ntree = c(50,100,200,500,700),
           maxnodes = c(5,10,15,20,30,50,100),
           nodesize = c(1,2,5,10))


mycost_auc <- function(y,yhat){
  roccurve=pROC::roc(y,yhat)
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
  
  k=cvTuning(randomForest,y~ ID, 
             data =bank_train,
             tuning =params,
             folds = cvFolds(nrow(bank_train), K=10, type ="random"),
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
ci.rf.final=randomForest(y~.-ID,
                         mtry=35,
                         ntree=700,
                         maxnodes=100,
                         nodesize=2,
                         data=bank_train)

ci.rf.final

test.predicted <- predict(ci.rf.final,newdata = bank_test)

write.csv(test.predicted,"Abdeali_Mithaiwala_P5_part2.csv",row.names = F)























