data=read.csv("Combined dataset_Sent to Jayanth 200918.csv")
X_data=data[,3:72]
X_data$Visit.quarter<-NULL #Removing Quater, since redundant
y_data=data[,73:74]

# Data Checking
for (i in colnames(X_data)){
  print(i)
  print(summary(X_data[[i]]))
}
# There is a missing data in Sa02..lowest
