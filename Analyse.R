data=read.csv("Combined dataset_Sent to Jayanth 200918.csv")
X_data=data[,3:72]
X_data$Visit.quarter<-NULL #Removing Quater, since redundant
X_data$Neck...FROM<-NULL #Told to remove by Joshua
X_data$Eyes...PEARL<-NULL #Told to remove by Joshua
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

#Cluster parameter selection using silhoutee
pos_clus=c(2,3,4)
clus_wid=c(0)
avg_wid=c(0)
for (i in pos_clus){
t_lab=cutree(h_tree,k=i)
v=silhouette(t_lab,dist=X_dist,full=TRUE,fun=mean)
t_s=summary(v)
clus_wid=rbind(clus_wid,paste(i,toString(t_s$clus.avg.widths)))
avg_wid=rbind(avg_wid,paste(i,t_s$avg.width))
}

#Max silhoutee width is for 3 clusters
labels=cutree(h_tree,k=3)
#Comparing for the outcomes between the clusters
y_data$cluster<-labels
pop1<-subset(y_data,cluster==1)
pop2<-subset(y_data,cluster==2)
pop3<-subset(y_data,cluster==3)
boxplot(pop1$POCT.CRP,pop2$POCT.CRP,pop3$POCT.CRP)
t<-kruskal.test(POCT.CRP ~ cluster,data=y_data)
t

x<-rbind(summary(pop1$VirusYes),summary(pop2$VirusYes),summary(pop3$VirusYes))
fisher.test(x)

#For 2 clusters
#Charaterzing the clusters
X_data$cluster<-factor(labels)
pop1<-subset(X_data,cluster==1)
pop2<-subset(X_data,cluster==2)
dif<-list()
for (i in 1:67){
  if(class(X_data[,i])=="integer" | class(X_data[,i])=="numeric"){
    form=as.formula(paste(names(X_data)[i],"cluster",sep="~"))
    t=wilcox.test(form,data=X_data)
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

#For more than 2 clusters
#Charaterzing the clusters
library(dunn.test)
X_data$cluster<-factor(labels)
pop1<-subset(X_data,cluster==1)
pop2<-subset(X_data,cluster==2)
pop3<-subset(X_data,cluster==3)
dif<-list()
for (i in 1:67){
  if(class(X_data[,i])=="integer" | class(X_data[,i])=="numeric"){
    form=as.formula(paste(names(X_data)[i],"cluster",sep="~"))
    t=kruskal.test(form,data=X_data)
    #Multiply the p-value value by 2 to account for both side testing
    dif[[colnames(X_data)[i]]]<-(t$p.value)
  }
  else{
    # For categorical data comparision
    le=levels(factor(X_data[,i]))
    pop1_r1<-table(factor(pop1[,i],levels=le))
    pop2_r2<-table(factor(pop2[,i],levels=le))
    pop3_r3<-table(factor(pop3[,i],levels=le))
    c_t<-rbind(pop1_r1,pop2_r2,pop3_r3)
    f<-fisher.test(c_t,simulate.p.value = TRUE)
    dif[[colnames(X_data)[i]]]<-(f$p.value)
  }
}

#Choosing factors with Alpha value of 0.05
ch_dif<-dif[dif<0.05]
names(ch_dif)

