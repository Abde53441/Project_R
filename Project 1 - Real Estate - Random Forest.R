options(scipen = 999,stringsAsFactors = F)

library(dplyr)

housing_train=read.csv("C:/Users/Humza Ali/Documents/Data/Project 1 - Real Estate/housing_train.csv",stringsAsFactors = F)

housing_test= read.csv("C:/Users/Humza Ali/Documents/Data/Project 1 - Real Estate/housing_test.csv",stringsAsFactors = F)


dim(housing_train)
head(housing_train)

dim(housing_test)
head(housing_test)

glimpse(housing_train)

sapply(housing_train,function(x) sum(is.na(x)))

housing_train$Bedroom2[is.na(housing_train$Bedroom2)]=median(housing_train$Bedroom2,na.rm=T)
housing_train$Bathroom[is.na(housing_train$Bathroom)]=round(mean(housing_train$Bathroom,na.rm=T),0)
housing_train$Car[is.na(housing_train$Car)]=round(mean(housing_train$Car,na.rm=T),0)
housing_train$Landsize[is.na(housing_train$Landsize)]=round(mean(housing_train$Landsize,na.rm=T),0)
housing_train$BuildingArea[is.na(housing_train$BuildingArea)]=round(mean(housing_train$BuildingArea,na.rm=T),0)
housing_train$YearBuilt[is.na(housing_train$YearBuilt)]=round(mean(housing_train$YearBuilt,na.rm=T),0)

sapply(housing_test,function(x) sum(is.na(x)))

housing_test$Bedroom2[is.na(housing_test$Bedroom2)]=median(housing_test$Bedroom2,na.rm=T)
housing_test$Bathroom[is.na(housing_test$Bathroom)]=round(mean(housing_test$Bathroom,na.rm=T),0)
housing_test$Car[is.na(housing_test$Car)]=round(mean(housing_test$Car,na.rm=T),0)
housing_test$Landsize[is.na(housing_test$Landsize)]=round(mean(housing_test$Landsize,na.rm=T),0)
housing_test$BuildingArea[is.na(housing_test$BuildingArea)]=round(mean(housing_test$BuildingArea,na.rm=T),0)
housing_test$YearBuilt[is.na(housing_test$YearBuilt)]=round(median(housing_test$YearBuilt,na.rm=T),0)


setdiff(names(housing_train),names(housing_test))

housing_test$Price=NA

housing_train$data = "train"
housing_test$data = "test"

housing_all = rbind(housing_train,housing_test,fill = TRUE)

dim(housing_all)

head(housing_all)
tail(housing_all)


glimpse(housing_all)

apply(housing_all,2,function(x)sum(is.na(x)))

#Create dummy vars for categorical vars
t=table(housing_all$Suburb)
View(t)
t1=round(tapply(housing_all$Price,housing_all$Suburb,mean,na.rm=T),0)
View(t1)
t1=sort(t1)

housing_all=housing_all %>% 
  mutate(
    sub_1=as.numeric(Suburb%in%c("Campbellfield","Jacana")),
    sub_2=as.numeric(Suburb%in%c("Kealba","Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner")),
    sub_3=as.numeric(Suburb%in%c("Glenroy","Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
    sub_4=as.numeric(Suburb%in%c("Airport West","Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
    sub_5=as.numeric(Suburb%in%c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
    sub_6=as.numeric(Suburb%in%c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
    sub_7=as.numeric(Suburb%in%c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
    sub_8=as.numeric(Suburb%in%c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
    sub_9=as.numeric(Suburb%in%c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
    sub_10=as.numeric(Suburb%in%c("Williamstown","East Melbourne","Seaholme")),
    sub_11=as.numeric(Suburb%in%c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
    sub_12=as.numeric(Suburb%in%c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
    sub_13=as.numeric(Suburb%in%c("Brighton East","Eaglemont","Hampton")),
    sub_14=as.numeric(Suburb%in%c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
    sub_15=as.numeric(Suburb%in%c("Brighton","Middle Park")),
    sub_16=as.numeric(Suburb%in%c("Albert Park","Balwyn","Malvern"))
  ) %>% 
  select(-Suburb)

glimpse(housing_all)

#Delete unique address
housing_all=housing_all %>% 
  select(-Address)

table(housing_all$Type)

housing_all=housing_all %>%
  mutate(Type_t=as.numeric(Type=="t"),
         type_u=as.numeric(Type=="u"))

housing_all=housing_all %>% 
  select(-Type)

glimpse(housing_all)

table(housing_all$Method)

housing_all=housing_all %>%
  mutate(Method_PI=as.numeric(Method=="PI"),
         Method_SA=as.numeric(Method=="SA"),
         Method_SP=as.numeric(Method=="SP"),
         Method_VB=as.numeric(Method=="VB")) %>% 
  select(-Method)

glimpse(housing_all)


t=table(housing_all$SellerG)
sort(t)


housing_all=housing_all %>%
  mutate(Gnelson=as.numeric(SellerG=="Nelson"),
         GJellis=as.numeric(SellerG=="Jellis"),
         Ghstuart=as.numeric(SellerG=="hockingstuart"),
         Gbarry=as.numeric(SellerG=="Barry"),
         GMarshall=as.numeric(SellerG=="Marshall"),
         GWoodards=as.numeric(SellerG=="Woodards"),
         GBrad=as.numeric(SellerG=="Brad"),
         GBiggin=as.numeric(SellerG=="Biggin"),
         GRay=as.numeric(SellerG=="Ray"),
         GFletchers=as.numeric(SellerG=="Fletchers"),
         GRT=as.numeric(SellerG=="RT"),
         GSweeney=as.numeric(SellerG=="Sweeney"),
         GGreg=as.numeric(SellerG=="Greg"),
         GNoel=as.numeric(SellerG=="Noel"),
         GGary=as.numeric(SellerG=="Gary"),
         GJas=as.numeric(SellerG=="Jas"),
         GMiles=as.numeric(SellerG=="Miles"),
         GMcGrath=as.numeric(SellerG=="McGrath"),
         GHodges=as.numeric(SellerG=="Hodges"),
         GKay=as.numeric(SellerG=="Kay"),
         GStockdale=as.numeric(SellerG=="Stockdale"),
         GLove=as.numeric(SellerG=="Love"),
         GDouglas=as.numeric(SellerG=="Douglas"),
         GWilliams=as.numeric(SellerG=="Williams"),
         GVillage=as.numeric(SellerG=="Village"),
         GRaine=as.numeric(SellerG=="Raine"),
         GRendina=as.numeric(SellerG=="Rendina"),
         GChisholm=as.numeric(SellerG=="Chisholm"),
         GCollins=as.numeric(SellerG=="Collins"),
         GLITTLE=as.numeric(SellerG=="LITTLE"),
         GNick=as.numeric(SellerG=="Nick"),
         GHarcourts=as.numeric(SellerG=="Harcourts"),
         GCayzer=as.numeric(SellerG=="Cayzer"),
         GMoonee=as.numeric(SellerG=="Moonee"),
         GYPA=as.numeric(SellerG=="YPA")
  ) %>% 
  select(-SellerG)

glimpse(housing_all)


table(housing_all$CouncilArea)

housing_all=housing_all %>%
  mutate(CA_Banyule=as.numeric(CouncilArea=="Banyule"),
         CA_Bayside=as.numeric(CouncilArea=="Bayside"),
         CA_Boroondara=as.numeric(CouncilArea=="Boroondara"),
         CA_Brimbank=as.numeric(CouncilArea=="Brimbank"),
         CA_Darebin=as.numeric(CouncilArea=="Darebin"),
         CA_Glen_Eira=as.numeric(CouncilArea=="Glen Eira"),
         CA_Monash=as.numeric(CouncilArea=="Monash"),
         CA_Melbourne=as.numeric(CouncilArea=="Melbourne"),
         CA_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
         CA_Manningham=as.numeric(CouncilArea=="Manningham"),
         CA_Kingston=as.numeric(CouncilArea=="Kingston"),
         CA_Hume=as.numeric(CouncilArea=="Hume"),
         CA_HobsonsB=as.numeric(CouncilArea=="Hobsons Bay"),
         CA_MoonValley=as.numeric(CouncilArea=="Moonee Valley"),
         CA_Moreland=as.numeric(CouncilArea=="Moreland"),
         CA_PortP=as.numeric(CouncilArea=="Port Phillip"),
         CA_Stonnington=as.numeric(CouncilArea=="Stonnington"),
         CA_Whitehorse=as.numeric(CouncilArea=="Whitehorse"),
         CA_Yarra=as.numeric(CouncilArea=="Yarra")) %>% 
  select(-CouncilArea)

glimpse(housing_all)


train=housing_all %>% filter(data=='train') %>% select(-data)
test=housing_all %>% filter(data=='test') %>% select(-data,-Price)


set.seed(2)
s=sample(1:nrow(train),0.7*nrow(train))
train_70=train[s,]
test_30=train[-s,]

dim(train)
dim(train)[1]*.7

dim(train_70)
dim(test_30)



fit=lm(Price ~ .,data=train_70)

library(car)

sort(vif(fit),decreasing = TRUE)

fit=lm(Price ~ .-sub_3 -Postcode,data=train_70)

sort(vif(fit),decreasing = TRUE)

summary(fit)

fit = step(fit)


fit =lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + 
          BuildingArea + YearBuilt + sub_1 + sub_2 + sub_5 + sub_6 + 
          sub_7 + sub_8 + sub_9 + sub_10 + sub_11 + sub_12 + sub_13 + 
          sub_14 + sub_15 + sub_16 + Type_t + type_u + Method_PI + 
          Method_SP + Method_VB + GJellis + GMarshall + GRT + GMcGrath + 
          GKay + GDouglas +  CA_Banyule +  CA_Brimbank + CA_Darebin + CA_Glen_Eira + 
          CA_Maribyrnong + CA_Manningham +  CA_HobsonsB + 
          CA_MoonValley + CA_Moreland + CA_PortP + CA_Yarra, data=train_70)


summary(fit)


library(randomForest)
library(cvTools)

train$Price <- as.factor(train$Price)
glimpse(train)

sapply(train,function(x) sum(is.na(x)))

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))


## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

## 

num_trials=50
my_params=subset_paras(param,num_trials)
my_params
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 50

## cvtuning for regression
## this code might take too long to run
## no need to execute completely in class
myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,Price~., 
             data =train,
             tuning =params,
             folds = cvFolds(nrow(train), K=10, type ="random"),
             seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}

## from another run following values were obtained


## Final model with obtained best parameters

housing.rf.final=randomForest(Price~.,
                         mtry=,
                         ntree=,
                         maxnodes=,
                         nodesize=,
                         data=train)

test.pred=predict(housing.rf.final,newdata = housing_test)

write.csv(test.pred,"Abdeali_Mithaiwala_P1_part2.csv",row.names = F)


































































