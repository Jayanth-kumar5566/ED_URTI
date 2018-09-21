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

#Gower distance and clustering
library(cluster)
X_dist=daisy(X_data,metric="gower")
h_tree=hclust(X_diss,method = "ward.D2")
labels=cutree(h_tree,k=2)

#Comparing for the outcomes between the clusters
y_data$cluster<-labels
pop1<-subset(y_data,cluster==1)
pop2<-subset(y_data,cluster==2)
boxplot(pop1$POCT.CRP,pop2$POCT.CRP)
t<-kruskal.test(POCT.CRP ~ cluster,data=y_data)
t

x<-rbind(summary(pop1$VirusYes),summary(pop2$VirusYes))
fisher.test(x)
