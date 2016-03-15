##Mariah Harvey
##February 29th, 2016
##K-means analysis as part of capstone project


library(tidyr)
library(data.table)
library(cluster)
library(fpc)
library(dplyr)

## Read in data
data<-read.csv("data_km.csv")
head(data)
names<-data.frame(data$Id2, data$Geography)
data<-data[,3:44]

## Set seed
set.seed(98)

## Perform Principal Component Analysis
pcadata<- prcomp(data, center=TRUE, scale. = TRUE)
percentvar<-pcadata$sd^2/sum(pcadata$sd^2)*100
percentvar[1:3]
screeplot(pcadata)
plot(pcadata, type="lines")
pcomp <- data.frame(pcadata$x[,1:3])
##Scale data
##data_scaled<- scale(data) 

# Determine number of clusters for k means
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(data, 
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="# of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis with k=3
fit <- kmeans(pcomp, 3) 
# get cluster means 
aggregate(pcomp,by=list(fit$cluster),FUN=mean)
##Plot clusters against 2 comps
mydata<-data.frame(pcomp, fit$cluster)
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

##Export file for shapefile plot
write.csv(knamesdata, "data_shapefile.csv")

## Set up diff in diff model

## read in employment variable = Y and make delta Y
employment<-read.csv("employment0809.csv")
head(employment)
head(knamesdata)
knamesdata <- merge(knamesdata,employment,by="data.Id2")

## Add in race variables
controls<-read.csv("Data_Collection.csv")
head(controls)
knamesdata$White<-((data$Estimate..White.alone/controls$Population)*100)
knamesdata$Black<-((data$Estimate..Black.or.African.American.alone/controls$Population)*100)
knamesdata$Hispanic<-controls$percent_hispanic
knamesdata$Asian<-((data$Estimate..Asian.alone/controls$Population)*100)

##Add in all other control variables
knamesdata$median_age<-controls$Total..Estimate..Total.population...SUMMARY.INDICATORS...Median.age..years
knamesdata$gender_ratio<-controls$Total..Estimate..Total.population...SUMMARY.INDICATORS...Sex.ratio..males.per.100.females.
knamesdata$education<-controls$Total..Estimate..Population.25.years.and.over...Bachelor.s.degree
knamesdata$married<-controls$Now.married..except.separated...Estimate..Population.15.years.and.over
knamesdata$population<-controls$Population

head(knamesdata)
## Only complete cases (theres should be 5 less obs because of 2008 employment var)
knamesdata<-knamesdata[complete.cases(knamesdata),]

## Seperate out data by k-clusters
group1<-filter(knamesdata, fit.cluster==1)
group2<-filter(knamesdata, fit.cluster==2)
group3<-filter(knamesdata, fit.cluster==3)

## Descriptive statistics for clusters
library(stargazer)
group1table<-stargazer(group1[,9:17])
group2table<-stargazer(group2[,9:17])
group3table<-stargazer(group3[,9:17])

## Create models
##Group 1
group1_1<-lm(deltaemploy~statedummy, data=group1)
group1_2<-lm(deltaemploy~statedummy+White+Black+Hispanic+Asian, data=group1)
group1_3<-lm(deltaemploy~statedummy+White+Black+Hispanic+Asian+education+married, data=group1)
##Group 2
group2_1<-lm(deltaemploy~statedummy, data=group2)
group2_2<-lm(deltaemploy~statedummy+White+Black+Hispanic+Asian, data=group2)
group2_3<-lm(deltaemploy~statedummy+White+Black+Hispanic+Asian+education+married, data=group2)
## Group 3
group3_1<-lm(deltaemploy~statedummy, data=group3)
group3_2<-lm(deltaemploy~statedummy+White+Black+Hispanic+Asian, data=group3)
group3_3<-lm(deltaemploy~statedummy+White+Black+Hispanic+Asian+education+married, data=group3)
##All Metro Groups
group4_1<-lm(deltaemploy~statedummy, data=knamesdata)
group4_2<-lm(deltaemploy~statedummy+White+Black+Hispanic+Asian, data=knamesdata)
group4_3<-lm(deltaemploy~statedummy+White+Black+Hispanic+Asian+education+married, data=knamesdata)

##Create output tables
stargazer(group1_1, group1_2, group1_3, title="Difference-In-Difference for Group 1")
stargazer(group2_1, group2_2, group2_3, title="Difference-In-Difference for Group 2")
stargazer(group3_1, group3_2, group3_3, title="Difference-In-Difference for Group 3")
stargazer(group4_1, group4_2, group4_3, title="Difference-In-Difference for All Metro Areas")
