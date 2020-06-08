setwd("C:/Practical Machine Learning Course")
library(readr)
library(dplyr)
library(kernlab)
library(caret) # machine learning model trainer - Classification and Regression Training
library(ISLR)
library(randomForest)
library(ggplot2)
library(ElemStatLearn)
#importing data
m1<-read.csv("pml-training.csv",header = T,stringsAsFactors = F)
m2<-read.csv("pml-testing.csv",header = T,stringsAsFactors = F)


nsv<- nearZeroVar(m1)
m1<- m1[,-nsv]
m2<- m2[,-nsv]

na_col <- sapply(m1, function(x) mean(is.na(x),na.rm = T)) > 0.95


m1 <- m1[,na_col == FALSE]
m2 <- m2[,na_col == FALSE]
str(m1)
m1<-m1[,8:59]
m2<- m2[,8:59]
  
dim(training)

inTrain<- createDataPartition(y=m1$classe,p=0.75,list=F) 
training<- m1[inTrain,]
testing<- m1[-inTrain,]
training$classe<- as.factor(training$classe)

str(m1)

set.seed(12345)
mod1<- train(classe~.,data=training,method="gbm",verbose=F) #0.9594 accuracy with gbm 
p1<- predict(mod1,testing)
testing$classe<- as.factor(testing$classe)
confusionMatrix(p1,testing$classe)

#now using validation
p2<- predict(mod1,newdata=m2)
summary(p2)
m2$classe<- p2 # updating the values in pml-testing csv 
write.csv(m2,"pml-final submission.csv",row.names=FALSE)

