# install.packages("e1071")
library("e1071")
str(iris)

# iris 데이터를 train과 test로 나눔
library(caret)
set.seed(100)
train_index = createDataPartition(y = iris$Species, p=0.7, list=FALSE)
train = iris[train_index,]
test = iris[-train_index,]

# 간단하게 SVM 모델 만들기
svm_model = svm(Species~., data=train)
summary(svm_model)

# predict 해보기
pred = predict(svm_model,test)
confusionMatrix(pred, test$Species)

# kernel에 따른 최적의 parameter 찾기 
tune1 = tune.svm(Species~. ,data=train, gamma=10^(-5:0),cost = 2^(0:4), kernel="radial")
tune2 = tune.svm(Species~., data=train, cost=2^(0:4), kernel="linear")
tune3 = tune.svm(Species~., data=train, cost=2^(0:4), degree=2:4, kernel = "polynomia")

tune1 # gamma 0.1 / cost 2
tune2 # cost 1
tune3 # degree 3 / cost 8

# 찾은 최적값으로 모델 다시 만들기
model_1 = svm(Species~., data=train, kernel="radial", cost=2, gamma=0.1)
summary(model_1)

model_2 = svm(Species~., data=train, kernel="linear", cost=1)
summary(model_2)

model_3 = svm(Species~., data=train, kernel="polynomia", cost=8, degree=3)
summary(model_3)

# 서포트벡터 확인(몇 번째 관찰값이 서포트 벡터일까!)
model_1$index
model_2$index
model_3$index

# predict 해보기
pred_1 = predict(model_1, test)
pred_2 = predict(model_2, test)
pred_3 = predict(model_3, test)


# 정확도
confusionMatrix(pred_1, test$Species)
confusionMatrix(pred_2, test$Species)
confusionMatrix(pred_3, test$Species)

# 시각화
plot(model_3, train, Petal.Width~ Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=5))

plot(model_3, train, Sepal.Width~ Sepal.Length,
     slice = list(Petal.Width=2.5, Petal.Length=3))
