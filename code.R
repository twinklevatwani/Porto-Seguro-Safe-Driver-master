## Read the train data, considering -1 as NA
df<-read.csv("F:\\kaggle\\logistic\\train.csv",na.strings = -1)
test<-read.csv("F:\\kaggle\\logistic\\test.csv",na.strings = -1)
View(df)
View(test)
summary(df)
summary(test)

## Converting -1 in continous variables to NA in train
df$ps_reg_03<-ifelse(as.integer(df$ps_reg_03)==-1,NA,df$ps_reg_03)
df$ps_car_12<-ifelse(as.integer(df$ps_car_12)==-1,NA,df$ps_car_12)
df$ps_car_14<-ifelse(as.integer(df$ps_car_14)==-1,NA,df$ps_car_14)

## Converting -1 in continous variables to NA in test
test$ps_reg_03<-ifelse(as.integer(test$ps_reg_03)==-1,NA,test$ps_reg_03)
test$ps_car_12<-ifelse(as.integer(test$ps_car_12)==-1,NA,test$ps_car_12)
test$ps_car_14<-ifelse(as.integer(test$ps_car_14)==-1,NA,test$ps_car_14)

##Converting factors to categorical variables
colnames(df)
col_train<-c(2,4,6,7,8,9,10:16,18,19,20,24:33,35,54:59)
col_test<-col_train[-1]-1
df[col_train]<-lapply(df[col_train],as.factor)
test[col_test]<-lapply(test[col_test],as.factor)

## removing the columns having more than 25% NAs in train
##, ie 1,50,000 NAs, and removing the corresponding columns in test as well.
lapply(df,function (x) sum(is.na(x)))
df<-df[,-c(26,28)]
test<-test[,-c(25,27)]

summary(df)
summary(test)

df1<-df
test1<-test
## NA values replacing with mean in continuous, and by mode in categorical for train.
i<-1
for(i in 1:ncol(df1))
{
  if(is.factor(df1[,i]))
  {
    t<- table(df1[,i])
    df1[,i][is.na(df1[,i])]=as.integer(names(t))[(t==max(t))]
  }
  if(is.numeric(df1[,i]))
  {
    df1[,i][is.na(df1[,i])]=mean(df1[,i],na.rm = T)
  }
}


## NA values replacing with mean in continuous, and by mode in categorical for test.
i<-1
for(i in 1:ncol(test1))
{
  if(is.factor(test1[,i])==T)
  {
    t<- table(test1[,i])
    test1[,i][is.na(test1[,i])]=as.integer(names(t))[(t==max(t))]
  }
  if(is.numeric(test1[,i])==T)
  {
    test1[,i][is.na(test1[,i])]=mean(test1[,i],na.rm = T)
  }
}

summary(df1)
summary(test1)

model<-glm(target~.,data = df1,family = binomial(link = "logit"))
summary(model)
results<-predict(model,test1,type = "response")

View(test1)
class(test1$id)
x<-cbind(test1$id,results)
colnames(x)<-c("id","target")
write.csv(x,file="submission.csv", row.names = F)

