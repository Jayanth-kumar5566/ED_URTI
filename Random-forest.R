# Random Forest Implementation for feature Importance
data=read.csv("Combined dataset_Sent to Jayanth 200918.csv")
#Removing missing values
data<-na.omit(data)
data=data[,3:74]
data$Visit.quarter<-NULL #Removing Quater, since redundant

# Data Checking
for (i in colnames(data)){
  print(i)
  print(summary(data[[i]]))
}

#Missing data in Sa02 lowest

library(randomForest)
#To predict POCT.CRP
rf<-randomForest(POCT.CRP ~ . -VirusYes,data = data,importance = TRUE)
varImp(rf)