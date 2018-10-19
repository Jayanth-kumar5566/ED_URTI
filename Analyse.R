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
h_tree=hclust(X_dist,method = "ward.D2")
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

#Charaterzing the clusters
X_data$cluster<-labels
pop1<-subset(X_data,cluster==1)
pop2<-subset(X_data,cluster==2)
dif<-list()
for (i in 1:69){
  if(class(X_data[,i])=="integer" | class(X_data[,i])=="numeric"){
    t=wilcox.test(pop1[,i],pop2[,i])
    #Multiply the p-value value by 2 to account for both side testing
    dif[[colnames(X_data)[i]]]<-(t$p.value)*2
  }
  else{
    # For categorical data comparision
    le=levels(factor(X_data[,i]))
    pop1_r1<-table(factor(pop1[,i],levels=le))
    pop2_r2<-table(factor(pop2[,i],levels=le))
    c_t<-rbind(pop1_r1,pop2_r2)
    f<-fisher.test(c_t,simulate.p.value = TRUE)
    dif[[colnames(X_data)[i]]]<-(f$p.value)
  }
}

#Choosing factors with Alpha value of 0.05
dif[dif<0.05]
