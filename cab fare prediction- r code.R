rm(list=ls())
#-----------  set working directory  ------------------

setwd("C:/Users/mitta/Desktop/project")
getwd()

# ---  install and load required libraries  ---------------

x=c('lubridate','tidyverse','caret',"Metrics",'rpart','randomForest')
install.packages(x)
lapply(x, require, character.only = TRUE) 


#------------ Load train and test data  ----------------

train=read.csv("./train_cab/train_cab.csv",na.strings = c('NA',""))
#Here we use na strings because in the data there are balnk cells which we should treat as NA
test=read.csv("./test/test.csv")

train=data.frame(train)

head(train)
head(test)

#--------  find any missing values in the data -----------
missing_val=data.frame(apply(train,2,function(x){sum(is.na(x))}))
print(missing_val)

#percentage of missing values
percentage_missing=(sum(missing_val[1])/nrow(train))*100
print(percentage_missing)

#percentage missing is less than 1%
#so remove the records contain null
train=train[complete.cases(train), ]

# convert fare amount into integer as it is in factor first convert into character then numeric
train$fare_amount=as.numeric(as.character(train$fare_amount))

# Find any missing values after conversion
missing_val=data.frame(apply(train,2,function(x){sum(is.na(x))}))
print(missing_val)

#one value set as NA after conversion due to coersion (430-) so set modified value 
train[1124,1]=430

train=train[complete.cases(train), ]

missing_val=data.frame(apply(train,2,function(x){sum(is.na(x))}))
print(missing_val)

# in test data there are no null values
missing_val=data.frame(apply(test,2,function(x){sum(is.na(x))}))
print(missing_val)

# In general passenger count >6 and <1 are considered as outliers remove them
train=train[which(!train$passenger_count<1),]
train=train[which(!train$passenger_count>6),]


#-- fare_amount is too large in few records and less than 0 , remove those records
train=train[which(!train$fare_amount>454),]
train=train[which(!train$fare_amount<=0),]


# -----   feature extraction------
# using pickup_datetime feature extract the additional features
# first convert the feature into datetime format later extract new features 
train$pickup_datetime =gsub(" UTC","",train$pickup_datetime)

train$Date <- as.Date(train$pickup_datetime)
train$Year <- substr(as.character(train$Date),1,4)
train$Month <- substr(as.character(train$Date),6,7)
train$Weekday <- weekdays(as.POSIXct(train$Date), abbreviate = F) 
train$Hour=substr(as.character(train$pickup_datetime),12,13)

train$Month=as.integer(train$Month)
train$Year=as.integer(train$Year)
train$Hour=as.integer(train$Hour)
train$Date=NULL

# similarly do it on the test data

test$pickup_datetime =gsub(" UTC","",test$pickup_datetime)

test$Date <- as.Date(test$pickup_datetime)
test$Year <- substr(as.character(test$Date),1,4)
test$Month <- substr(as.character(test$Date),6,7)
test$Weekday <- weekdays(as.POSIXct(test$Date), abbreviate = F) 
test$Hour=substr(as.character(test$pickup_datetime),12,13)

test$Month=as.integer(test$Month)
test$Year=as.integer(test$Year)
test$Hour=as.integer(test$Hour)
test$Date=NULL


# now using pickup and dropoff latitudes and longitudes details calculate distance
lat1 = train['pickup_latitude']
lat2 = train['dropoff_latitude']
long1 = train['pickup_longitude']
long2 = train['dropoff_longitude']

lat1=lat1/57.29577951
lat2=lat2/57.29577951

long1=long1/57.29577951
long2=long2/57.29577951

dlon = long2 - long1  
dlat = lat2 - lat1 
a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
c = 2 * asin(sqrt(a))  
r=c*6371

train$distance=r$dropoff_latitude
summary(train$distance)

# now on seeing the data there is sudden change in distance values
#upto 130 that is reasonable after that it looks like outliers, remove them
train=train[which(!train$distance>130),]
train=train[which(!train$distance==0),]
train=train[which(!train$distance<0),]

# similarly do it on the test data

lat1 = test['pickup_latitude']
lat2 = test['dropoff_latitude']
long1 = test['pickup_longitude']
long2 = test['dropoff_longitude']

lat1=lat1/57.29577951
lat2=lat2/57.29577951

long1=long1/57.29577951
long2=long2/57.29577951

dlon = long2 - long1  
dlat = lat2 - lat1 
a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
c = 2 * asin(sqrt(a))  
r=c*6371

test$distance=r$dropoff_latitude
summary(test$distance)

# now remove all unwanted columns for training the data
train=subset(train,select=-c(pickup_datetime,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
test=subset(test,select=-c(pickup_datetime,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))


train=as.data.frame(train)
train=train[complete.cases(train), ]

# find out if there is any skewness in the data
ggplot(train,aes_string(x=train$Month))+geom_histogram(fill="cornsilk",colour="black")+
  geom_density()+theme_bw()+xlab(" Month")
#in month data it is uniform
ggplot(train,aes_string(x=train$Year))+geom_histogram(fill="cornsilk",bins=15,colour="black")+
  geom_density()+theme_bw()+xlab(" Year")
# in year also it is not skewed much 
ggplot(train,aes_string(x=train$distance))+geom_histogram(fill="cornsilk",colour="black")+
geom_density()+theme_bw()+xlab(" distance")
# but in distance values it is skewed much so apply log transform to normalize

train$distance=log1p(train$distance)

ggplot(train,aes_string(x=train$distance))+geom_histogram(fill="cornsilk",colour="black")+
  geom_density()+theme_bw()+xlab(" distance")
#Now skeawness decreases much

# Now convert weekday feature factor into numerical using technique label encoder
train$Weekday=factor(train$Weekday,labels=(1:length(levels(factor(train$Weekday)))))
test$Weekday=factor(test$Weekday,labels=(1:length(levels(factor(test$Weekday)))))


# prepare data for training and testing
X=train[1:1200,]

X_test=train[1201:nrow(train),]

#linear regression


X_test=train[1201:nrow(train),]

lm_model=lm(fare_amount~.,data=X)
summary(lm_model)

# from the summary of the model year is the most important feature

predictions_LR=predict(lm_model,X_test[,2:7])
rmse(predictions_LR,X_test$fare_amount)
mape(predictions_LR,X_test$fare_amount)
mae(predictions_LR,X_test$fare_amount)



# Decision Tree
DT=rpart(fare_amount~.,data=X)
predictions_DT=predict(DT,X_test[,2:7])
rmse(predictions_DT,X_test$fare_amount)
mape(predictions_DT,X_test$fare_amount)
mae(predictions_DT,X_test$fare_amount)



#Random forest

rf=randomForest(fare_amount~.,data=X)
predictions_rf=predict(rf,X_test[,2:7])
rmse(predictions_rf,X_test$fare_amount)
mape(predictions_rf,X_test$fare_amount)
mae(predictions_rf,X_test$fare_amount)

# cross validation of ntree for random forest
for (i in c(70,80,90,100,120,130,140,160,180,200,220,300,400,500)){
  rf=randomForest(fare_amount~.,data=X,ntree=i)
  print("For ntree=")
  print(i)
  print(rmse(predict(rf,X_test[,2:7]),X_test$fare_amount))
  print(mape(predict(rf,X_test[,2:7]),X_test$fare_amount))
  print(mae(predict(rf,X_test[,2:7]),X_test$fare_amount))
}

# from the observations ntree =90 is the best case


# ----------Apply Randomforest on final data -------------------
rf=randomForest(fare_amount~.,data=train,ntree=90)
final_predict_test=as.data.frame(predict(rf,test))

test$fare_amount=final_predict_test[1]
write.csv(test,"new_test.csv",row.names=T)



