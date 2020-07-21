## 1번
setwd('C:/Users/danan/Desktop/Bitamin/ML_R/reviews')
data = read.csv('heart.csv')
str(data)
library(caret)
set.seed(4)
train_index = createDataPartition(data$AHD, p=0.7, list=FALSE)
df_train = data[train_index,]
df_test = data[-train_index,]

## 2번
library(rpart)
#2-1
accuracy = c()
library(dplyr)
for (i in 1:20) {
  set.seed(234)
  model = rpart(AHD~., minsplit=i, data = df_train)
  predicted = model %>%  predict(df_test,"class")
  acc = mean(predicted == df_test$AHD)
  accuracy = c(accuracy, acc)
}
print(accuracy)
# accuracy가 가장 큰 i는 20이군!

#2-2
set.seed(234)
ahd_rpart = rpart(AHD~.,minsplit=20, data = df_train)
plot(ahd_rpart); text(ahd_rpart, cex=0.8)

#2-3
printcp(ahd_rpart)
plotcp(ahd_rpart)
ahd_prune = prune(ahd_rpart, cp =ahd_rpart$cptable[which.min(ahd_rpart$cptable[,"xerror"]),"CP"])
plot(ahd_prune); text(ahd_prune)

## 3번
ahd_pred = predict(ahd_rpart, df_test, type='class')
confusionMatrix(ahd_pred, df_test$AHD)

ahd_prune_pred = predict(ahd_prune, df_test, type='class')
confusionMatrix(ahd_prune_pred, df_test$AHD)
