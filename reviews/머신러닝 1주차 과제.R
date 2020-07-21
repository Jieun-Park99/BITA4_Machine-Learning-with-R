rm(list=ls())
data = read.csv("C:/Users/danan/Desktop/비타민/머신러닝_R/insurance2.csv")
data[,c("sex", "smoker","region")] = lapply(data[, c("sex","smoker","region")], factor)
data$bmi[is.na(data$bmi)] = mean(data$bmi, na.rm=T)
data$bmi30 = as.factor(ifelse(data$bmi >=30, "비만", "정상"))

set.seed(123)
index<-sample(c("train","test"),size=nrow(data),replace=T,prob=c(0.7, 0.3))
table(index)

train<-data[index=="train",]
test<-data[index=="test",]
train
test


install.packages("caret")
library(caret)
set.seed(123)
cv_list = createFolds(train$charges, k=5)
head(cv_list)


