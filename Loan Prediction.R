options(scipen=999)# Scientific penalty to show numbers not in scientific format. 999 is the maximum precision in R
options(stringsAsFactors = FALSE,scipen = 999)

library(tidyverse)
library(data.table)
library(caret)
library(ggplot2)
library(e1071)
library(forecast)
library(broom)
library(devtools)
library(vegan)
library(stringr)
library(stringi)
library(psych)
library(vcd)
library(dplyr)
library(randomForest) 
library(tidyr)
library(tree)
library(pROC)
library(cvTools)
library(car)

getwd()


loan_train=read.csv("C:/Users/Humza Ali/OneDrive/Documents/Data/Practise/loan_train.csv",stringsAsFactors = F)

loan_test= read.csv("C:/Users/Humza Ali/OneDrive/Documents/Data/Practise/loan_test.csv",stringsAsFactors = F)


head(loan_train)

head(loan_test)

dim(loan_train)
dim(loan_test)

glimpse(loan_train)

setdiff(names(loan_train),names(loan_test))

loan_test$Loan_Status <- NA

loan_train$data <- "train"
loan_test$data <- "test"

loan_all <- rbind(loan_train,loan_test)

head(loan_all)

glimpse(loan_all)

table(loan_all$Loan_Status)

loan_all$Loan_Status = ifelse(loan_all$Loan_Status =='Y',1,0)

loan_all$Loan_Status = as.numeric(loan_all$Loan_Status)

table(loan_all$Loan_Status)

table(loan_all$Dependents)

loan_all = loan_all %>%
  mutate(Dependents=ifelse(Dependents=="3+",3,substr(Dependents,1,1)),
         Dependents=as.numeric(Dependents))

table(loan_all$Dependents)

sapply(loan_all, function(x) sum(is.na(x)))

glimpse(loan_all)

loan_all$Dependents[is.na(loan_all$Dependents)] = round(mean(loan_all$Dependents,na.rm = TRUE),0)

loan_all$LoanAmount[is.na(loan_all$LoanAmount)] = round(mean(loan_all$LoanAmount,na.rm = TRUE),0)

loan_all$Loan_Amount_Term[is.na(loan_all$Loan_Amount_Term)] = round(mean(loan_all$Loan_Amount_Term,na.rm = TRUE),0)

loan_all$Credit_History[is.na(loan_all$Credit_History)] = round(mean(loan_all$Credit_History,na.rm = TRUE),0)

sapply(loan_all, function(x) sum(is.na(x)))

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

names(loan_all)[sapply(loan_all,function(x) is.character(x))]

cat_cols=c("Gender","Married","Education","Self_Employed","Property_Area")

for(cat in cat_cols){
  loan_all=CreateDummies(loan_all,cat,100)
}

glimpse(loan_all)

loan_train=loan_all %>% filter(data=="train") %>% select(-data)
loan_test=loan_all %>% filter(data=="test") %>% select(-data,-Loan_Status)

set.seed(2)
s=sample(1:nrow(loan_train),0.8*nrow(loan_train))
loan_train1=loan_train[s,]
loan_train2=loan_train[-s,]

dim(loan_train1)
dim(loan_train2)

library(car)

for_vif=lm(Loan_Status~.-Loan_ID,data=loan_train1)

sort(vif(for_vif),decreasing = T)[1:3]

log_fit <- glm(Loan_Status~.-Loan_ID,data=loan_train1,family="binomial")

log_fit=step(log_fit)

log_fit=glm(Loan_Status ~  Credit_History + Married_Yes + 
               Property_Area_Semiurban,
            data=loan_train,family='binomial')

summary(log_fit)

library(pROC)

val.score=predict(log_fit,newdata = loan_train2,type='response')

auc(roc(loan_train2$Loan_Status,val.score))

train.score1 = predict(log_fit, newdata = loan_train1, type="response")

auc(roc(loan_train1$Loan_Status,train.score1))

val.score=predict(log_fit,newdata = loan_train,type='response')

auc(roc(loan_train$Loan_Status,val.score))

train.score1 = predict(log_fit, newdata = loan_train2, type="response")

auc(roc(loan_train2$Loan_Status,train.score1))

## DECISION TREES & RANDOM FOREST

train_80_1 <- copy(loan_train1)
train_80_1$Loan_Status <- as.factor(train_80_1$Loan_Status)
train_80_1$Loan_ID <- NULL
rf.model3= randomForest(Loan_Status~.-Loan_ID,data=train_80_1)
test.score3=predict(rf.model3,newdata=loan_train2,type="prob")[,2]
auc(roc(loan_train2$Loan_Status,test.score3))

library(cvTools)

loan_train$Loan_Status=as.factor(loan_train$Loan_Status)

glimpse(loan_train)

param=list(mtry=c(3,4,6,8,10),
           ntree=c(50,100,200,500,700,800,900), 
           maxnodes=c(5,10,15,20,30,50,100,300,500,600,700),
           nodesize=c(1,2,5,10,20,30,40)       
)

mycost_auc=function(Loan_Status,Loan_Statushat){  #Real #Predicted
  roccurve=pROC::roc(Loan_Status,Loan_Statushat)
  score=pROC::auc(roccurve)
  return(score)
}  

subset_paras=function(full_list_para,n=10){  #n=10 is default, you can give higher value
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trial=50
my_params=subset_paras(param,num_trial)
my_params

myauc=0

for(i in 1:num_trial){  
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,
             Loan_Status~.-Loan_ID, 
             data =loan_train,
             tuning =params,
             folds = cvFolds(nrow(loan_train), K=15, type ="random"),
             cost =mycost_auc, 
             seed =2,
             predictArgs = list(type="prob"))
  
  score.this=k$cv[,2]
  
  
  if(score.this>myauc){
    #print(params)
    #uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    #uncomment the line above to keep track of progress
    #print(myauc)
    best_params=params
  }
  #print('DONE')
}

best_params
##      mtry ntree maxnodes nodesize
## 1076    3   800      500        5

ci.rf.final=randomForest(Loan_Status~.-Loan_ID,
                         mtry=3,
                         ntree=800,
                         maxnodes=500,
                         nodesize=5,
                         data=loan_train
)

myauc

test.score_final=predict(ci.rf.final,newdata=loan_test)

test.score_final =data.frame(data = test.score_final)

names(test.score_final) <- c('Loan_Status')

test.score_final["Loan_Status"] = ifelse(test.score_final["Loan_Status"]==1,"Y","N")

test.score_final$Loan_ID <- loan_test$Loan_ID


test.score_final

write.csv(test.score_final,'Loan_Prediction_R.csv',row.names = F)

