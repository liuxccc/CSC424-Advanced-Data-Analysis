fifa<-read.csv("/Users/xiaochangliu/Desktop/FianlProject/FIFA17.csv")
head(fifa)
summary(fifa)

#Drop Name, Nationality, National_Position, National_Kit
#Drop Club, Club_Position, Club_Kit, Birth_Date, Preffered_Position
data<-fifa[-c(1,2,3,4,5,6,7,14,16)]
head(data)
summary(data)
#Convert Club_Joining into years that player are in the team
CYear<-as.Date(data$Club_Joining,'%m/%d/%Y')
Club_Year<-as.numeric(format(CYear,'%Y'))
No_of_Years_In_Club<-2017-Club_Year

#Convert contract_expiry into number of years
No_of_Years_for_Contract_Expiry<-data$Contract_Expiry-2017

#Add No_of_Years_In_Club and No_of_Years_for_Contract_Expiry
data1<-cbind(data,No_of_Years_In_Club)
data2<-cbind(data,No_of_Years_for_Contract_Expiry)
data3<-data2[-c(1,2)]
head(data3)

#Filling in missing values
data3$No_of_Years_In_Club[is.na(data3$No_of_Years_In_Club)] <- median(data3$No_of_Years_In_Club, na.rm = TRUE)
data3$No_of_Years_for_Contract_Expiry[is.na(data3$No_of_Years_for_Contract_Expiry)] <- median(data3$No_of_Years_for_Contract_Expiry, na.rm = TRUE)

#remove strings from height and weight
Height=as.numeric(gsub("cm","",data3$Height))
Weight=as.numeric(gsub("kg","",data3$Weight))
#Add Height and Weight to dataset
data4<-data3[-c(2,3)]
data5<-cbind(data4,Height)
data6<-cbind(data5,Weight)

#Create dummy variables for preffered_foot
data6$Preffered_Foot<-as.factor(data6$Preffered_Foot)
PrefferedFoot<-model.matrix(~Preffered_Foot-1,data=data6)
data6<-cbind(data6,PrefferedFoot)

#Create dummy variables for Work_Rate
WorkRate <- model.matrix(~ Work_Rate - 1, data = data6)
data6 <- cbind(data6, WorkRate)
data7<-data6[-c(2,4)]
head(data7)

#standardize variables
mydata<-na.omit(data7)
mydata<-scale(mydata)
head(mydata)
summary(mydata)

#determine number of clusters
clusternum<-(nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:51) clusternum[i]<-sum(kmeans(mydata, centers=i)$withinss)
plot(1:51,clusternum,type="b",xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#K-means Cluster analysis
fit<-kmeans(mydata,centers=7,nstart=10)
fit
fit$cluster
aggregate(mydata,by=list(fit$cluster),FUN=mean)
mydata<-data.frame(mydata,fit$cluster)
#Cluster Plot for first two principal components
library(cluster)
clusplot(mydata,fit$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#Hierarchical Clustering
d<-dist(mydata,method="euclidean")
fit2<-hclust(d,method="ward.D")
plot(fit2)

#fit model based on clustering
library(mclust)
fit3<-Mclust(mydata)
summary(fit3)
plot(fit3)
plotcluster(mydata,fit3$cluster)
