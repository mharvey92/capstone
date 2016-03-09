##Mariah Harvey
##February 29th, 2016
##K-means analysis as part of capstone project


library(tidyr)
library(data.table)
library(cluster)
library(fpc)

## Read in data
data<-read.csv("data_km.csv")
head(data)
names<-data.frame(data$Id2, data$Geography)
data<-data[,3:44]

## Perform Principal Component Analysis
pcadata<- prcomp(data, center=TRUE, scale. = TRUE)
percentvar<-pcadata$sd^2/sum(pcadata$sd^2)*100
percentvar[1:3]
screeplot(pcadata)
plot(pcadata, type="lines")

##Scale data
data<- scale(data) 

# Determine number of clusters for k means
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(data, 
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="# of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis with k=3
fit <- kmeans(data, 3) # 5 cluster solution
# get cluster means 
aggregate(data,by=list(fit$cluster),FUN=mean)
##Plot clusters against 2 comps
mydata<-data.frame(data, fit$cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# append cluster assignment to df
knamesdata <- data.frame(names, fit$cluster)
head(knamesdata)

##Create dummy variable for affected vs. unaffected
knamesdata$statedummy<-0

affected<-c('AL', 'AK', 'AR', 'DE', 'FL', 'GA', 'ID', 'IN', 'KS', 'LA', 'MD', 'MN', 'MS', 'MO', 'MT', 'NE', 'NJ', 'NY', 'NC', 'ND', 'OK', 'PA', 'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'WV', 'WI', 'WY')

for(x in affected){
  for(i in 1:nrow(knamesdata)){
    if(length(grep(x, knamesdata$data.Geography[i]))>0)
      knamesdata$statedummy[i] <- 1
    }
}

## Set up diff in diff model

## read in employment variable = Y and make delta Y
employment<-read.csv("employment0809.csv")
head(employment)
knamesdata$deltaemploy<-(employment$Employment_total09-employment$Employment_total08)

## Add in control variables
knamesdata$white<-mydata$Estimate..White.alone
knamesdata$black<-mydata$Estimate..Black.or.African.American.alone
knamesdata$indian<-mydata$Estimate..American.Indian.and.Alaska.alone
knamesdata$asian<-mydata$Estimate..Asian.alone

