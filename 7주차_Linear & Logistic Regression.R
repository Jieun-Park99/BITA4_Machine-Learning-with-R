library(MASS)
data("Boston", package="MASS")
data = Boston
str(data)

stem(data$medv)

i = which(data$medv==50)
boston = data[-i,] #주택 최대값 50인 자료 빼기
boston$chas = factor(boston$chas) #factor 형식으로 바꾸기 
boston$rad = factor(boston$rad)

cor(boston[c("zn","nox","rm","age","dis","tax","ptratio","black","crim","indus","medv")])

m = lm(medv~rm, data=boston)
summary(m)
plot(boston$rm, boston$medv)
abline(m)

summary(m)

lm_result = lm(medv~rm, data=boston)

#예측할 독립변수
room = c(6,7,8,9,10)
df_input = data.frame(rm=room)

#예측
predict_medv = predict(lm_result, df_input, interval="confidence", level=0.95)

#결과
cbind(df_input, predict_medv)

###### 다중선형회귀 모델훈련
m2 = lm(medv~., data=boston)
summary(m2)

par(mfrow=c(2,2))
plot(m2)

m2.both = step(m2, direction="both") #foward와 backward
m2.both
summary(m2.both)

## 다중공선성 확인하기
m2.slm = step(m2, direction="both")
library(car)
vif(m2.slm)
#수치형은 좌측 GVIF df는 범주의 개수 rad는 8인건 곧
#rad는 8개의 범주를 갖고 있다!
summary(m2.slm)

#예측해보기
pre_medv = predict(m2.slm, boston, interval="confidence")
pre_medv = as.data.frame(pre_medv)
pre_medv$actual = boston$medv
head(pre_medv)
summary(pre_medv$actual)
rmse = with(pre_medv, sqrt(sum((actual-fit)^2)/nrow(boston)))
rmse

par(mfrow=c(1,1))
plot(pre_medv$fit, pre_medv$actual, xlim=c(0,50), ylim=c(0,50), xlab="predict", ylab="actual")
abline(a=0, b=1, col="red",lwd=3)

######### 로지스틱 회귀 #########
president = read.csv("C:/Users/danan/Desktop/Bitamin/ML_R/president.csv",header=T)
dim(president)
str(president)
apply(president[,c(1,3,5,6)],2,unique)

## reference groupo(기준그룹) ##
levels(president$대선92)
president$대선92 = relevel(president$대선92, ref="클링턴")
levels(president$대선92)
president$성별 = relevel(president$성별, ref='여자')
president$학력 = relevel(president$학력, ref="대학원")

## multinom model 구축 ##
#install.packages("nnet")
library(nnet)
presi.multi = multinom(대선92~학력+성별+학력:성별, data=president) #full model
step.multi = step(presi.multi,direction="both",trace=F) #단계선택법법

multi.sum = summary(step.multi)
multi.sum

## pvalue 구하기 ##
z = multi.sum$coefficients/multi.sum$standard.errors
p = round((1 - pnorm(abs(z),0,1))*2,3)
p

## exp(b)값 구하기 ##
round(exp(multi.sum$coefficients),3)

## model 예측 ##
head(fitted(step.multi)) #각 범주에 속할 확률

#predict(step.multi, president,type="probs") #같은 방법 
pred = predict(step.multi,president)

library(caret)
confusionMatrix(pred, president$대선92)
