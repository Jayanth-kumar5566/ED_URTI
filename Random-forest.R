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
library(caret)

#Splitting the dataset into testing and training
library(caTools)
poct_m=c()
imp_crp=list()
imp_virus=list()
spec_sens=0
acc=0
for (i in 1:100){
set.seed(runif(1,0,1e9))
#To predict POCT.CRP
rf<-randomForest(POCT.CRP ~ . -VirusYes,data = data,importance = TRUE)
print(rf)
#Importance
imp_crp[[i]]<-varImp(rf)
#Metrics
poct_m=c(poct_m,tail(rf$rsq,1))

#To predict VirusYes
#training
msk = sample.split(data$VirusYes, SplitRatio=3.2/4)
tr_data=data[msk,]
tst_data=data[!msk,]
rf<-randomForest(VirusYes ~ . ,data = tr_data,importance = TRUE)
#prediction of testing
prediction_for_table <- predict(rf,tst_data)
c_s=confusionMatrix(prediction_for_table,tst_data$VirusYes,positive = "Virus")
#Importance
imp_virus[[i]]<-varImp(rf)
#Metrics
spec_sens=rbind(spec_sens,c_s$byClass[1:2])
acc=rbind(acc,c_s$overall[1])
}
spec_sens=spec_sens[-1]
acc=acc[-1]

av_imp_crp=imp_crp[[1]]
av_imp_virus=imp_virus[[i]]
for (i in 2:100){
  av_imp_crp=av_imp_crp+imp_crp[[i]]
  av_imp_virus=av_imp_virus+imp_virus[[i]]
}
av_imp_crp=av_imp_crp/100
av_imp_virus=av_imp_virus/100
