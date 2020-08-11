## 문제에 들어가기 전에 먼저 데이터 전처리 하기
# 데이터 읽기

#데이터 파일을 filepath에 저장
filepath <- "https://quantdev.ssri.psu.edu/sites/qdev/files/student-mat.csv"
#url() function을 이용해서 파일 열기
data <- read.table(file=url(filepath),sep=";",header=TRUE)

#모든변수 이름을 소문자로 하기
var.names.data <-tolower(colnames(data))
colnames(data) <- var.names.data
head(data)


#필요한 라이브러리
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)

data_class <- data


# mjob을 예측하려 함(그래서 따로 데이터를 빼줌)
#mjob_outcome <- data_class %>% select(mjob)

# mjob을 뺀 데이터
#data_class <- data_class %>% select(-mjob)
#str(data_class)

##세 가지 이상의 level 없애기, yes/no factor(level이 numeric인 거) 없애기
##필요한 부분만 쓰기
data_class = data_class %>% select(age, traveltime, studytime,failures,health,school,sex,address,famsize,mjob)
head(data_class)
str(data_class)

## 문제 1-1 = data_class를 정규화 시켜주기 - 그리고 더미화도 시켜주기
normalize = function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}
nums = as.data.frame(lapply(select(data_class,-c(school,sex,address,famsize,mjob)),normalize))
head(nums,10)
data_class[,c("age","traveltime","studytime","failures","health")] = nums
head(data_class)


data_class$school = dummy.code(data_class$school)
data_class$sex = dummy.code(data_class$sex)
data_class$address = dummy.code(data_class$address)
data_class$famsize = dummy.code(data_class$famsize)
str(data_class)

## 문제 1-2 = 정규화와 dummy화한 데이터를 train, test set나누기(비율은 train:test=7:3)train, test set 나누기 전에 set.seed(1234) 사용하기
set.seed(1234)
train_index = createDataPartition(data_class$mjob, p=0.7, list=F)
data_train = data_class[train_index,]
data_test = data_class[-train_index,]

## 문제 1-3 = 모델링을 통해 confusionMatrix 도출하기 (number=10, repeats=2로 지정)
tt = trainControl(method='repeatedcv', number=10, repeats=2)
set.seed(123)
knnFit1 = train(mjob~., data=data_train, method='knn',
                trControl=tt, metric='Accuracy',tuneLength=10)
knnFit1
knnPredict1 = predict(knnFit1, newdata = data_test)
cmat1 = confusionMatrix(knnPredict1, data_test$mjob)
cmat1

## 데이터를 다시 원상복귀하고 이번에는 scale(표준화)를시켜주겠습니다.
data_class <- data
mjob_outcome <- data_class %>% select(mjob)
data_class <- data_class %>% select(-mjob)
data_class<-data_class %>% select(age, traveltime, studytime,failures,health,school,sex,address,famsize)
data_class[, c("age","traveltime", 
               "studytime", "failures", "health")] <- scale(data_class[, c("age", "traveltime", "studytime", "failures", "health")])
head(data_class)

##Kmeans 연속형 처리 –Kmeans 함수에 연속형(수치형) 변수들만을 가져와 돌려보려 합니다. mjob을 기준으로 나누어 보려 합니다. (아직 문제 아님, 문제 소개입니다)
#mjob_outcome 빼주었던 것을 다시 합쳐줌
pre_kmeans<-cbind(data_class,mjob_outcome)
str(pre_kmeans)

#수치형만 변수로 가져와줌
num_data<-pre_kmeans[,-c(6:9)]
head(num_data)

## 문제 2-1 = 엘보식을 이용하여 그래프를 그려봅니다. 그리고 엘보식으로 구한 k값으로 kmeans 함수를 돌려봅니다. 정확성도 확인해 봅니다.
## 엘보식
set.seed(123)
# k = 15까지만 돌려보겠습니다
k_max = 15
wss = sapply(1:k_max, 
              function(k){kmeans(num_data[,-6], k, iter.max = 1000 )$tot.withinss})
plot(1:k_max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

##kmeans 해보기
head(num_data)
model = kmeans(num_data[,-6],centers=5, iter.max=10000)

##정확성 확인 
class(num_data)
class(model$cluster)
num_data$train_cluster = as.factor(model$cluster)
table(num_data$mjob, num_data$train_cluster)

##Kmeans 범주형 –Kmeans 개념을 차용한 kmodes 함수를 사용해 보려 합니다. //factor인 변수들만 가져와주기(mjob을 기준으로 clustering을 해줄 것이기 때문에 factor인 mjob은 포함하지 않음)
##준비
str(pre_kmeans)
fac_data = pre_kmeans[,6:9]
str(fac_data)

## 문제 2-2 = K-modes 함수를 이용해 clustering을 해 봅니다. 그리고 data$mjob과 kmodes 함수를 사용해 나온 cluster를 비교해 정확성을 확인합니다. (kmodes함수 사용 전 set.seed(123)써주기)
set.seed(123)

library(klaR)
cl = kmodes(fac_data,5)
cl

fac_data$kmodes_cluster = as.factor(cl$cluster)
table(data$mjob, fac_data$kmodes_cluster)

