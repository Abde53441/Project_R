options(scipen = 999,stringsAsFactors = F)

setwd("C:/Users/Humza Ali/Documents/Data")

getwd()


hr_train=read.csv("C:/Users/Humza Ali/Documents/Data/Project 2 - Human Resource/hr_train.csv",stringsAsFactors = F)

hr_test= read.csv("C:/Users/Humza Ali/Documents/Data/Project 2 - Human Resource/hr_test.csv",stringsAsFactors = F)

dim(hr_train)
dim(hr_test)

head(hr_train)


setdiff(names(hr_train),names(hr_test))

hr_test$left <- NA

hr_train$data <- "train"
hr_test$data <- "test"

hr_all <- rbind(hr_train,hr_test)

library(dplyr)

glimpse(hr_all)

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

names(hr_all)[sapply(hr_all,function(x) is.character(x))]

cat_cols=c("sales","salary")

for(cat in cat_cols){
  hr_all=CreateDummies(hr_all,cat,300)
}

glimpse(hr_all)

sum(sapply(hr_all,function(x) is.character(x)))

names(hr_all)[colSums(is.na(hr_all))>0]

hr_train=hr_all %>% filter(data=="train") %>% select(-data)
hr_test=hr_all %>% filter(data=="test") %>% select(-data,-left)  

set.seed(2)
s=sample(1:nrow(hr_train),0.8*nrow(hr_train))
hr_train1=hr_train[s,]
hr_train2=hr_train[-s,]

library(car)

for_vif=lm(left~.,data=hr_train1)

sort(vif(for_vif),decreasing = T)[1:3]


for_vif=lm(left~.-sales_sales,data=hr_train1)

sort(vif(for_vif),decreasing = T)[1:3]

log_fit <- glm(left~.-sales_sales,data=hr_train1,family="binomial")

log_fit=step(log_fit)

log_fit=glm(left ~ satisfaction_level + last_evaluation + number_project + 
              average_montly_hours + time_spend_company + Work_accident + 
              promotion_last_5years + sales_hr + sales_accounting + sales_marketing + 
              sales_IT + sales_technical + salary_medium + salary_low,
            data=hr_train,family='binomial')

summary(log_fit)

log_fit=glm(left ~ satisfaction_level + last_evaluation + number_project + 
              average_montly_hours + time_spend_company + Work_accident + 
              promotion_last_5years + sales_hr + sales_accounting + 
              salary_medium + salary_low,
            data=hr_train,family='binomial')

summary(log_fit)

library(pROC)

val.score=predict(log_fit,newdata = hr_train2,type='response')

auc(roc(hr_train2$left,val.score))

train.score1 = predict(log_fit, newdata = hr_train1, type="response")

auc(roc(hr_train1$left,train.score1))

caTools::colAUC(val.score,hr_train2$left, plotROC = TRUE)

train.score = predict(log_fit, newdata = hr_train, type = "response")

real=hr_train$left

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

test.predicted <- as.numeric(predict(log_fit, hr_test) > my_cutoff)

hr_train$train_predicted <- train.predicted

hr_test$test_predicted <- test.predicted

library(caret)

confusionMatrix(factor(hr_train$train_predicted),factor(hr_train$left))

