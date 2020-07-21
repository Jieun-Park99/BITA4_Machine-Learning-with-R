library(MASS)
library(caret)

set.seed(1234)
train_idx = createDataPartition(Boston$medv, p=0.7, list=F)

Boston_train = Boston[train_idx,]
Boston_test = Boston[-train_idx,]

# 1.RandomForest 실행
library(randomForest)
set.seed(1234)
Boston_rf = randomForest(medv~.,data=Boston_train)
Boston_rf
# 트리가 500개 생성! 
# 개별트리 하나에서 노드는 4개로 고려변수를 줄여!
# MSE는 OOB데이터(모든모델에서 한번도 쓰이지 않은 것)로 계산됨


# 2.회귀트리ntree=1 vs 랜덤포레스트ntree=500
library(rpart)
set.seed(1234)
Boston_reg.tr = rpart(medv~., data=Boston_train)
Boston_reg.tr_pred = predict(Boston_reg.tr, Boston_test)
Boston_rf_pred = predict(Boston_rf, Boston_test)

MSE = function(actual, predicted){
  mean((actual-predicted)^2)
  # 여기서 mean을 쓰는건데 원래는 자유도로 나눠줘야흔ㄴ데 잘 모르겠어서 mean 씀
  
}

MSE(Boston_test$medv, Boston_reg.tr_pred)
MSE(Boston_test$medv, Boston_rf_pred)

# 랜덤포레스트가 MSE가 더 작네

# 회귀트리 모형
plot(Boston_test$medv, Boston_reg.tr_pred,
     xlab = 'Observed Values', ylab='Fitted Values', main='Regression Tree (Single Model)')
abline(0, 1, col='red', lwd=2)

#랜덤포레스트 형
plot(Boston_test$medv, Boston_rf_pred,
     xlab = 'Observed Values', ylab='Fitted Values', main='Random Forest (Bagging Model)')
abline(0, 1, col='red', lwd=2)

plot(Boston_rf)
# 트리가 많아질수록 MSE가 감소하는걸 볼 수 있음
# 너무 많아지면 계산도 오래걸리고 과적합이 될 거같기도...

# 3. 배깅 vs 랜덤포레스트
p = length(Boston_train)-1 #p=독릾변수의 수 

#mtry=p : 배깅
set.seed(1234)
Boston_rf_p = randomForest(medv~., data=Boston_train, ntree=500, mtry=p)
Boston_rf_p_pred = predict(Boston_rf_p, Boston_test)

#랜덤포레스트 / mtry=5
set.seed(1234)
Boston_rf_5 = randomForest(medv~., data=Boston_train, ntree=500, mtry=5)
Boston_rf_5_pred = predict(Boston_rf_5, Boston_test)

MSE(Boston_rf_p_pred, Boston_test$medv)
MSE(Boston_rf_5_pred, Boston_test$medv)

# 이유는 개별 모델 간의 상관성때문일것같음?


# 붓스트랩은 복원추출로 뽑아 그럼 실제 통계량과 비슷해진다~
# 붓스트랩 셋 6개를 생성하고 6개의 개별트리를 학습시킨다
B_reg.rt = c()
for (i in 1:6){
  B_idx = sample(1:nrow(Boston_train), replace=T) #여기서 replace T가 중요 
  B_set = Boston_train[B_idx,]
  B_reg.rt[[i]] = rpart(medv~., data=B_set) #개별트리 하나 만듬 
}

# 각각의 개별트리의 플랏
library(rpart.plot)
par(mfrow=c(2,3))
for(i in 1:6){
  rpart.plot(B_reg.rt[[i]], cex=0.8)
}
par(mfrow=c(1,1))

# ntree는 분산이 낮아지지 다양성 확보
# mtry는 독립성이니까 분산이 낮아저 
# 랜포는 트리 500개를 다 파악하기 어려우니까 
# 분류 과정을 다 볼 수 없음 


#3. 변수 중요도
set.seed(1234)
Boston_rf_5 = randomForest(medv~., data=Boston_train, ntree=500, mtry=5, importance=T)
importance(Boston_rf_5)
# x mse(oob) 그리고 x를 섞어주고 MSE를 구하고 비교!
# 한 변수가 선택되면서 mse가 줄어드는 것들의 합을 다른 트리와 비교해서 평균을 냄..
varImpPlot(Boston_rf_5)
#%IncMSE : permutation(순서섞기)방ㅂ버으로 변수중요도 계산
#IncNodePurity: MSE의 감소합의 평균, 분류는 gini index

#tuneRF
features = setdiff(names(Boston_train), "medv") #medv 변수만 골라내기
set.seed(1234)
tuneRF(
  x=Boston_train[features], y=Boston_train$medv,
  ntreeTry=500, stepFactor=1.5, improve=0.01 # improve보다 안좋으면 멈추는 알고리즘임 
) #mtry=9에서 OOB Error 최소

#caret를 활용한 Hyperprarmeter 도출
fitControl = trainControl(method="repeatedcv", number=10, repeats=5)

set.seed(1234)
rf_fit = train(medv~., data=Boston_train, method="rf",
               trControl = fitControl)
rf_fit
plot(rf_fit) #mtry=13에서 RMSE 최소

#그리드 서치: 탐색 범위 직접 설정
customGrid = expand.grid(mtry=1:13)
set.seed(1234)
rf_fit2 = train(medv~., data=Boston_train,
                method="rf", trControl = fitControl,
                tuneGrid=customGrid)
rf_fit2
rf_fit2$results[which.min(rf_fit2$results$RMSE),]
plot(rf_fit2) #mtry=11에서 RMSE 최소 

#MSE를 구해봅시다
set.seed(1234)
boston_rf_9 = randomForest(medv~., data=Boston_train, ntree=500, mtry=9)
boston_rf_9_pred = predict(boston_rf_9, Boston_test)

boston_rf_fit2_pred = predict(rf_fit2, Boston_test)

MSE(Boston_test$medv, boston_rf_9_pred)
MSE(Boston_test$medv, boston_rf_fit2_pred)

# 데이터셋: AmesHousing 패키지의 make_ames() 셋
# train/ test 분할
library(rsample)
install.packages("AmesHousing")
set.seed(1234)
ames_split = initial_split(AmesHousing::make_ames(), prop=.7)
ames_train = training(ames_split)
ames_test = testing(ames_split)
str(ames_train)

#ranger 패키지
install.packages("ranger")
library(ranger)
#range() 실행
ames_ranger = ranger(
  formula = Sale_Price~.,
  data = ames_train,
  num.trees=500,
  mtry = floor(length(features)/3)
)
ames_ranger
ames_ranger_pred = predict(ames_ranger, ames_test)

#궁금하시면 randomForest 패키지와 실제로 속도차이가 있는지 확인해 보세요~!
#ames_rf = randomForest(Sale_Price~., data=ames_train)

#속도가 향상되었으니 이전보다 다양한 파라미터 조합으로 그리드(HP조합) 생성
hyper_grid = expand.grid(
  mtry = seq(20,30,by=2),
  num.trees = c(300,500,700,1000),
  node_size = seq(3, 9, by=2)
)
head(hyper_grid)
tail(hyper_grid)

for(i in 1:nrow(hyper_grid)){
  
  #train model
  model = ranger(
    formula = Sale_Price~.,
    data = ames_train,
    num.trees = hyper_grid$num.trees[i],
    mtry = hyper_grid$mtry[i],
    min.node.size = hyper_grid$node_size[i],
    seed = 1234
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] = sqrt(model$prediction.error)
} 

# RMSE(OOB Error)가 가장 낮은 Hyperparameter 조합
G = hyper_grid[which.min(hyper_grid$OOB_RMSE),]; G

# 하이퍼파라미터 넣어서 다시 fitting
optimal_ranger = ranger(
  formula = Sale_Price~.,
  data = ames_train,
  num.trees = G$num.trees,
  mtry = G$mtry,
  min.node.size = G$node_size,
  importance = 'permutation'
)
optimal_ranger_pred = predict(optimal_ranger, ames_test)

#MSE 감소 확인
MSE(ames_ranger_pred$predictions, ames_test$Sale_Price)
MSE(optimal_ranger_pred$predictions, ames_test$Sale_Price)

# 변수 중요도
optimal_ranger$variable.importance

library(dplyr)
#변수 중요도 플랏
imp = as.vector(optimal_ranger$variable.importance)
val = rownames(data.frame(optimal_ranger$variable.importance))
valimp = data.frame(val, imp)
valimp_tidy = valimp %>%  arrange(desc(imp)) %>%  top_n(25)
valimp_tidy
ggplot(valimp_tidy, aes(reorder(val,imp), imp))+geom_col()+coord_flip()
