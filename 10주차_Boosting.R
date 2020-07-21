#install.packages("adabag")
library(adabag)
data("iris")
str(iris) # 관측치 150개개

set.seed(300)
train = createDataPartition(iris$Species, p=0.7, list=F)

iris_ada = boosting(Species~., data=iris[train,], mfinal=10,
                    control=rpart.control(maxdepth=1))

# 자세히 살펴보기
print(names(iris_ada))
summary(iris_ada)
iris_ada$trees[1]
iris_ada$weights

result = data.frame(iris_ada$class, iris_ada$votes, iris_ada$prob)
result

iris_ada$importance
importanceplot(iris_ada)

p_iris_ada = predict(iris_ada, iris[-train,])
p_iris_ada$confusion
p_iris_ada$error

# errorval

evol.train = errorevol(iris_ada, newdata=iris[train,])
plot(evol.train)
abline(h=min(evol.train[[1]]), col="blue", lty=2, lwd=2)

evol.test = errorevol(iris_ada, newdata=iris[-train,])
plot.errorevol(evol.test, evol.train)
abline(h= min(evol.test[[1]]),col="red", lty=2, lwd=2)


#### credit data #####
setwd("C:/Bitamin/ML_R")
credit = read.csv("credit.csv")
str(credit)

set.seed(300)
credit_ada = boosting(default~., data=credit)

# 자세히 살펴보기
summary(credit_ada)

#rpart.control을 해주지 않았을 때
credit_ada$trees[1]

# 변수 중요도
credit_ada$importance

# 예측하기
p_credit_ada = predict(credit_ada, credit)
p_credit_ada$confusion #과적합 발생생
p_credit_ada$error

# 교차검증을 통해 과적합 방지
set.seed(300)
cv_adaboost = boosting.cv(default~., data=credit) #v: cv 수 조정(=k)
summary(cv_adaboost)

cv_adaboost$confusion
cv_adaboost$error


# 다른 모델과 성능 비교
train = sample(1:nrow(credit), nrow(credit)*0.7)
cr_train = credit[train,]
cr_test = credit[-train,]

# Regression tree
library(rpart)
set.seed(300)
reg.tr = rpart(default~., data=cr_train,method="class")
reg.tr_pred = predict(reg.tr, cr_test, type="class")
confusionMatrix(cr_test$default, reg.tr_pred)

# RandomForest
library(randomForest)
set.seed(300)
ranf.tr = randomForest(default~., data=cr_train)
ranf.tr_pred = predict(ranf.tr, cr_test, type="class")
confusionMatrix(cr_test$default, ranf.tr_pred)

# Bagging
set.seed(300)
bg.tr = randomForest(default~., data=cr_train, mtry=16)
bg.tr_pred = predict(bg.tr, cr_test, type="class")
confusionMatrix(cr_test$default, bg.tr_pred)

# Adaboosting
set.seed(300)
ada.tr = boosting(default~., data=cr_train)
ada.tr_pred = predict(ada.tr, cr_test) # cv로 하고 싶으나 boosting 함수 이용
a = as.factor(ada.tr_pred$class)
confusionMatrix(cr_test$default, a)
