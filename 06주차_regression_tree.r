library(caret)
library(rpart)
library(tree)
library(rpart.plot)
library(RWeka)


setwd("C:/Users/danan/Desktop/Bitamin/ML_R/3조_발표")


## Example : Predicting Price of Real Estate(using regression and model tree)
## Step1. Exploring and Splitting the data
apt <- read.csv("Daegu_Real_Estate_data.csv")
str(apt)
summary(apt)

options("scipen" = 100)   # change exponential notation to fixed notation
# format(apt$SalePrice, scientific = F) : limited local var
hist(apt$SalePrice)

# Splitting the data into train and test set
library(caret)

set.seed(408)

train_idx <- createDataPartition(apt$SalePrice, p = .7, list = F)
apt_train <- apt[train_idx,]
apt_test <- apt[-train_idx,]

hist(apt_train$SalePrice)
hist(apt_test$SalePrice)


## Step2. Training a model on the data
# Regression tree using rpart
library(rpart)

set.seed(408)

apt_reg.tr <- rpart(SalePrice ~ ., data = apt_train)
apt_reg.tr
summary(apt_reg.tr)

plot(apt_reg.tr); text(apt_reg.tr)

# Visualization for decision tree
library(rpart.plot)   # ref) "http://www.milbo.org/rpart-plot/"

rpart.plot(apt_reg.tr, digits = 4)
rpart.plot(apt_reg.tr, digits = 3, fallen.leaves = F, type = 4, extra = 101)


## Step3. Pruning the tree (for avoid overfitting)
printcp(apt_reg.tr)   # or apt_reg.tr$cptable
plotcp(apt_reg.tr)
apt_reg_prune <- prune.rpart(apt_reg.tr, cp = apt_reg.tr$cptable[8, "CP"])

rpart.plot(apt_reg_prune, digits = 3, fallen.leaves = F, type = 4, extra = 101)


## Step4. Evaluating model performance (using MAE)
apt_reg_pred <- predict(apt_reg.tr, apt_test)
summary(apt_reg_pred)
summary(apt_test$SalePrice)

cor(apt_reg_pred, apt_test$SalePrice)

# Function to calculate the mean absolute error(MAE)
MAE <- function(actual, predicted){
  mean(abs(actual - predicted))
}

MAE(apt_reg_pred, apt_test$SalePrice)

# Comparing orig.regression tree with pruned tree & training data
apt_reg_prune_pred <- predict(apt_reg_prune, apt_test)

cor(apt_reg_prune_pred, apt_test$SalePrice)   # not pruned : 0.917
MAE(apt_reg_prune_pred, apt_test$SalePrice)   #            : 32,035.81

mean(apt_train$SalePrice)   # result = 221280
MAE(221280, apt_test$SalePrice)


## Step5. Improving model performance
# Model tree using M5' algorithm
library(RWeka)

set.seed(408)

apt_mod.tr <- M5P(SalePrice ~ ., data = apt_train)
apt_mod.tr
# capture.output(apt_mod.tr, file = "filename") : text 파일 형태로 전체 출력결과 보기
summary(apt_mod.tr)

plot(apt_mod.tr)
# Error : argument is of length zero가 출력될 것임
# 잎 노드에 NA가 있어서 그런 것으로 추정됨

# Visualization for model tree
library(tree)

plot(tree(apt_mod.tr)); text(tree(apt_mod.tr))

# Evaluation model tree
apt_mod_pred <- predict(apt_mod.tr, apt_test)
summary(apt_mod_pred)
summary(apt_test$SalePrice)

cor(apt_mod_pred, apt_test$SalePrice)
MAE(apt_mod_pred, apt_test$SalePrice)

