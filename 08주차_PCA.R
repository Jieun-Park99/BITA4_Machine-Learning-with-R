#install.packages("HSAUR")
library(HSAUR)
library(car)
data(heptathlon)
head(heptathlon)
str(heptathlon)

plot(heptathlon)
cor(heptathlon)

# 다른 변수와 마찬가지로 양의 상관관계를  갖도록 값 변경
heptathlon$hurdles = max( heptathlon$hurdles ) - heptathlon$hurdles
heptathlon$run200m = max( heptathlon$run200m ) - heptathlon$run200m
heptathlon$run800m = max( heptathlon$run800m ) - heptathlon$run800m
plot(heptathlon)

# 다중공선성
fit = lm(score~., data=heptathlon)
summary(fit)
vif(fit)

pc.fit = princomp(subset(heptathlon, select=-score),cor=T,scores=T)
pc.fit$scores # 새로 계산된 행렬의 의미 (2행렬)

cor(pc.fit$scores) #상관계수 0

pc.fit$loadings # 주성분 계수, 0에 가까운 값은 빈칸, ss loadings 이부분은 요인분석 내용

summary(pc.fit)
screeplot(pc.fit, type="l", pch=19, main="screeplot") #2개만 선택택

biplot(pc.fit, scale=F, cex=0.7)

new_data = as.data.frame(cbind(pc.fit$scores[,1:2], heptathlon$score))
head(new_data)
colnames(new_data)[3] = "score"

pc.lm = lm(score~., data=new_data)
summary(pc.lm)

summary(fit)$adj.r.squared
summary(pc.lm)$adj.r.squared
# 다중공선성 문제 해결
vif(pc.lm) ; cor(new_data[,1:2])

svd.fit = prcomp(subset(heptathlon,select=-score),center=T,scale=T)
svd.fit$rotation
pc.fit$loadings

svd.fit$x # 주성분 점수, scores와 같음

vars = apply(svd.fit$x, 2, var)
props = vars / sum(vars)
props
cumsum(props)
