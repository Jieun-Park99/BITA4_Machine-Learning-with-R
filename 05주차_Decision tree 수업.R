setwd('C:/Users/danan/Desktop/Bitamin/ML_R')
### CREDIT (classification) - C5.0 (Set.seed(3))
library(C50)
credit = read.csv('credit.csv')
str(credit)
summary(credit)

# Dataset split
set.seed(3)
train_sample = sample(1000,900)
credit_train = credit[train_sample,]
credit_test = credit[-train_sample,]

# Train data와 Test data내의 default 비율 비슷한지 확인.
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# Building model
credit_model = C5.0(credit_train[-17], credit_train$default)
credit_model # Tree size = 69
summary(credit_model) # Error 10.4%

# Confusion matrix - 기본
credit_pred = predict(credit_model, credit_test)
caret::confusionMatrix(credit_pred, credit_test$default)

## C5.0 의사결정 트리 정확도 향상
## Adaptive boosting (AdaBoosting)
# Building model
credit_boost10 = C5.0(credit_train[-17], credit_train$default, trials=10)
credit_boost10 # Tree size = 56.6 (10회의 반복을 통해 트리의 크기가 줄어듬)
summary(credit_boost10) #Error 1.4%

## Penalty 부여
# Cost matrix
matrix_dimensions = list(c("no","yes"),c("no","yes")) #먼저 차원구성
names(matrix_dimensions) = c("predicted","actual")
matrix_dimensions
error_cost = matrix(c(0,1,4,0), nrow = 2,
                    dimnames = matrix_dimensions)
error_cost

# Building model
credit_cost = C5.0(credit_train[-17], credit_train$default,
                   cost = error_cost)
summary(credit_cost)

# Confusion matrix - Adaptive Boosting
credit_boost10_pred = predict(credit_boost10, credit_test)
caret::confusionMatrix(credit_boost10_pred, credit_test$default)

# Penalty Matrix
credit_cost_pred = predict(credit_cost, credit_test)
caret::confusionMatrix(credit_cost_pred, credit_test$default)


# Dataset split with caret package (Response variable 비율 유지)
library(caret)
set.seed(234)
train_index = createDataPartition(credit$default, p=0.7)$Resample1
credit_train = credit[train_index,]
credit_test = credit[-train_index,]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))


# Building model
library(rpart)
credit_rpart = rpart(default~., data = credit_train)
plot(credit_rpart) ; text(credit_rpart, cex = 0.8)
credit_rpart

# Pruning (과적합 방지)
printcp(credit_rpart)
plotcp(credit_rpart)
credit_prune = prune(credit_rpart, cp = credit_rpart$cptable[which.min(credit_rpart$cptable[,"xerror"]), "CP"])
plot(credit_prune) ; text(credit_prune)

# Test data에서 검정 : Confusion matrix
credit_pred = predict(credit_rpart, credit_test, type="class")
confusionMatrix(credit_pred, credit_test$default)

credit_prune_pred = predict(credit_prune, credit_test, type="class")
confusionMatrix(credit_prune_pred, credit_test$default)
