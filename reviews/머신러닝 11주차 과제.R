  ## 1-1 cancer 데이터를 createDataPartition을 k이용해 train:test = 7:3 으로 분할하세요 (set.seed=113)

library(caret)
setwd('C:/Users/danan/Desktop/Bitamin/ML_R/reviews')
data = read.csv('cancer.csv')
set.seed(113)
train_index = createDataPartition(data$y, p=0.7, list=F)
train = data[train_index,]
test = data[-train_index,]
head(train)
head(test)


## 1-2 RandomForest 모델을 사용하여 train으로 학습한 모델을 test셋에 대한 Accuracy를 구하세요. (옵션은 mtry=3, ntrees=113)

library(randomForest)
data_rf = randomForest(y~., data=train, mtry=3, ntrees=113)
data_rf_pred = predict(data_rf, test)
confusionMatrix(data_rf_pred,test$y)
confusionMatrix(data_rf_pred,test$y)$overall[1]
# 97%


## 1-3 AdaBoost 모델을 사용하여 train으로 학습한 모델을 test셋에 대한 Accuracy를 구하세요 (옵션 지정 X)

library(adabag)
data_ada = boosting(y~., data=train)
data_ada_pred = predict(data_ada, test)
predicted = as.factor(data_ada_pred$class)
actual=as.factor(test$y)
confusionMatrix(predicted,actual)$overall[1]
# 98%


## 2-1 수업시간에 배운 내용을 바탕으로 XGBoost를 사용하기 전 데이터를 적절한 매트릭스 형태로 바꿔주세요 (train/test 같은 방식)

library(xgboost)

train$y = as.numeric(train$y) -1
head(train$y)
test$y = as.numeric(test$y) -1
head(test$y)

library(dplyr)
x = model.matrix(~., data=train %>% select(-y))
train_mat = as.matrix(x[,-1])
xg.train = xgb.DMatrix(train_mat, label = train$y)
xg.train

x = model.matrix(~., data=test %>% select(-y))
test_mat = as.matrix(x[,-1])
xg.test = xgb.DMatrix(test_mat, label = test$y)
xg.test

## 2-2 XGBoost 모델로 Accuracy를 구하는 문제입니다. params는 주어진 조건대로 max_depth와 nrounds를 grid search 를 통해 가장 큰 Accuracy를 갖는 조합을 찾으세요 (max_depth의 범위는 1부터 6까지, nrounds의 조합은 seq(100,310,30)으로 설정해주세요!)

acc=c()


a = expand.grid(a=1:6, b=seq(100,310,30))
for (i in 1:nrow(a)){
  params = list(booster = "gbtree",
                eta = 0.01,
                max_depth = a$a[i] ,
                gamma =3 ,
                subsample = 0.8,
                objective = "binary:logistic",
                eval_metric = "error")
  xgb.fit = xgb.train(
    params = params,
    data=xg.train,
    nrounds= a$b[i],
    watchlist = list(val1=xg.train, val2=xg.test),
    verbose=0)
  xgb.pred = predict(xgb.fit, xg.test, reshape=T)
  xgb.pred = as.data.frame(xgb.pred)
  head(xgb.pred)
  xgb.pred["0"] = 1-xgb.pred
  colnames(xgb.pred) <- sort(levels(data$y),decreasing=T)
  
  xgb.pred$prediction = apply(xgb.pred,1,
                              function(x) colnames(xgb.pred)[which.max(x)])
  head(xgb.pred)
  xgb.pred$prediction = as.factor(xgb.pred$prediction)
  xgb.pred$label = levels(data$y)[test$y+1]
  xgb.pred$label = as.factor(xgb.pred$label)
  head(xgb.pred)
  ac = confusionMatrix(xgb.pred$prediction,xgb.pred$label)$overall[1]
  acc = c(acc,ac)
}

length(acc)
which.max(acc)
a[33,]
max(acc)
