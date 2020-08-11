wbcd = read.csv('C:/Users/danan/Desktop/Bitamin/ML_R/wbcd.csv')
str(wbcd)
library(ggplot2)
library(caret)
library(dplyr)
wbcd$diagnosis = factor(wbcd$diagnosis, levels = c("B","M"),
                        labels =c("Benign","Malignant"))
str(wbcd)



#정규화
normalize = function(x){
  return((x-min(x)) / (max(x)-min(x)))
}
normalize(c(1,2,3,4,5))
wbcd_n = as.data.frame(lapply(select(wbcd, -diagnosis), normalize))

train_index = createDataPartition(wbcd$diagnosis, p=0.7, list=FALSE)
wbcd_train = wbcd[train_index,]
wbcd_test = wbcd[-train_index,]

# 비율 확인 (종속label비율 확인)
frqtab = function(x, caption) {
  round(100*prop.table(table(x)), 1)
}

ft_orig = frqtab(wbcd$diagnosis)
ft_train = frqtab(wbcd_train$diagnosis)
ft_test = frqtab(wbcd_test$diagnosis)
ftcmp_df = as.data.frame(cbind(ft_orig, ft_train, ft_test))
ftcmp_df

### 정답 레이블 선정 ########
# k-fold교차검증
# number : number만큼 쪼개겠다.
# repeat : repeat 수만큼 반복
ctrl = trainControl(method="repeatedcv", number=10, repeats=2)

# data shuffle
set.seed(12345)

# caret에서 제공하는 preProcess : center(평균을 0), scale(표준화), range(정규화)
# 회기일 경우 metric : RMSE
knnFit1 = train(diagnosis ~., data = wbcd_train, method="knn",
                trControl=ctrl, metric="Accuracy", tuneLength = 10
                #preProc=c('range)
)
knnFit1

## 최적의 k 구하기
# k=5일때 최적
plot(knnFit1)
max(knnFit1$bestTune)

## 최적의 k로 예측
# 예측
knnPredict1 = predict(knnFit1, newdata=wbcd_test)
cmat1 = confusionMatrix(knnPredict1, wbcd_test$diagnosis,
                        positive = "Malignant")
cmat1


################## knn 다른 함수 ################################
# 정규화
normalize = function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

wbcd_n = as.data.frame(lapply(select(wbcd, -diagnosis), normalize))

#train / test
wbcd_train = wbcd_n[1:469,]
wbcd_test = wbcd_n[470:569,]

wbcd_train_labels = wbcd[1:469,1]
wbcd_test_labels=wbcd[470:569,1]

# knn
library(class)
wbcd_test_pred = knn(train = wbcd_train, test = wbcd_test,
                     cl = wbcd_train_labels, k =21)

library(gmodels)
#crosstable
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=TRUE)

#일반횟수
#카이제곱 (기대치 비율)
#행을 기준으로 비율 값( 가로로 읽는다.)
#컬럼을 기준으로 비율 값(세로로 읽는다.)
#전체를 기준으로 비율 값 
###################################################################

### 유의점1 ###
# 변수가 범주형일 경우 dummy화 시켜줘야 한다.
# 그 후 그대로 train, test 셋 나누고 모델 학습시켜주면 된다.
# Dummies(패키지)는 one-hot encoding 방식
Season = c("S1","S2","S3","S4","S1","S2","S3","S4")
store = c('a','b','c','a','a','b','c','c')
SalesAmt = c(300,800,400,100,280,750,390,60)
TS = data.frame(Season,store,SalesAmt,stringsAsFactors = F)
TS

### 유의점2 ###
library(dummies)
dummy.data.frame(select(TS, -c(SalesAmt)))

library(e1071)
dummy.code(TS$Season)
dummy.code(TS$store)



#####################################################################
install.packages("klaR")
library(klaR)

x = rbind(matrix(rbinom(250,2,0.25), ncol=5),
          matrix(rbinom(250,2,0.75), ncol=5))
colnames(x) = c("a","b","c","d","e")
View(x)

cl = kmodes(x,2)
cl
plot(jitter(x), col=cl$cluster)
points(cl$modes, col=1:5, pch=8)
