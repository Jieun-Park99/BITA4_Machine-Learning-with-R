# install.packages("xgboost")
library(xgboost)
# 데이터 불러오기(내장된 iris 데이터 사용)
data(iris)
head(iris)
str(iris)

# XGBoost는 class들이 0부터 시작하는 integer format으로 형성되어 있음 
# 따라서 그 형식에 맞게 바꿔준다!
species = iris$Species
label = as.integer(iris$Species)-1
iris$Species = NULL

set.seed(113)
n=nrow(iris)
train.index=sample(n,floor(0.7*n))
train.data = as.matrix(iris[train.index,])
train.label = label[train.index]
test.data = as.matrix(iris[-train.index,])
test.label = label[-train.index]
length(train.label)
length(test.label)

# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data, label=train.label)
xgb.test = xgb.DMatrix(data=test.data, label=test.label)
xgb.train
xgb.test

# 다른 데이터를 사용해서 한번 더 해보자!
setwd('C:/Users/danan/Desktop/Bitamin/ML_R')
data = read.csv('cancer.csv')
data
data$y = as.numeric(data$y) -1
head(data$y)
library(dplyr)
x = model.matrix(~., data=data %>% select(-y))
x = as.matrix(x[,-1])
x = as.data.frame(x)
train_mat = as.matrix(x[,-1])
xgb.matrix = xgb.DMatrix(train_mat, label=data$y)
xgb.matrix

# Define the parameters for multinomial classification
num_class = length(levels(species))
num_class
params = list(
  booster="gbtree", 
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Train the XGBoost classifier
xgb.fit = xgb.train(
  params = params,
  data=xgb.train,
  nrounds=2500,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train, val2=xgb.test),
  verbose=0
  #verbose = 1로 두면 each round마다 결과를 확인할 수 있다.
)

# Review the final model and results
xgb.fit

# Predict outcomes with the test data
xgb.pred = predict(xgb.fit, test.data, reshape=T)
xgb.pred = as.data.frame(xgb.pred)
xgb.pred

colnames(xgb.pred) = c("setosa","versicolor","virginica")
xgb.pred


# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$prediction = as.factor(xgb.pred$prediction)
xgb.pred$prediction

xgb.pred$label = levels(species)[test.label+1]
xgb.pred$label = as.factor(xgb.pred$label)
head(xgb.pred)

library(caret)
confusionMatrix(xgb.pred$prediction, xgb.pred$label)

var_mat = xgb.importance(feature_names = colnames(test.data), model=xgb.fit)
xgb.plot.importance(importance_matrix=var_mat)
                            
                            
                            