library(ade4)

data(olympic)
data<-olympic$tab
summary(data)

# 달리기의 경우 짧을수록 좋아서 추세를 맞추기 위헤 max에서 원래 값들을 다 빼줌

data$"100"<-max(data$'100') - data$'100'
data$'400'<-max(data$'400') - data$'400'
data$'110'<-max(data$'110') - data$'110'
data$'1500'<-max(data$'1500') - data$'1500'

cor(data)

## 1번
pca.cov = princomp(data,cor=F, scores=T)
pca.cor = princomp(data,cor=T, scores=T)
pca.cov$loadings
pca.cor$loadings
# 주성분 계수의 차이가 있다.
# 이유는 데이터를 centering 했냐 안했냐에 따라 달라지기 때문이다. cov의 경우 분산이 큰 1500달리기가 압도적으로 큰 비중을 차지하는 것을 볼 수 있다. 하지만 cor의 경우 골고루 분산된 것을 볼 수 있다.

# 공분산 분해를 공분산행렬로 했는지 상관계수행렬로 했는지에 따라 고유값과 고유행렬이 달라짐 
#만약 centering을 했다면 cov와 cor이 같아서 결과가 같다! 

## 2번
summary(pca.cor)
screeplot(pca.cor, type="l", pch=19, main="screeplot")
# 4로 할래 이유는 3과 4가 비슷하고 5로가면 또 다시 떨어지므로 3과 4까지 포함하면 충분히 설명 가능하다고 생각


## 3번
biplot(pca.cor, scale=F, cex=0.7)
#pc1은 전반적으로 큰 값을 갖음 - 전체적인 운동능력지표변수
#pc2는 고루 퍼져있음 딱히 이상치인 것은 없어보임 
#변수들이 가까운 거리와 방향으로 모여있는 것들이 많아 달리기는 달리기끼리 던지기는 던지기끼리 상관성이 높은 것을 볼 수 있음 

## 정답
# 높이뛰기(haut), 110m 허들(110), 멀리뛰기(long), 100미터 달리기(100), perc(장대높이뛰기)는 PC1과 강한 양의 부하량을 갖고있다.
# PC1은 점프력과 관련이 있는 성분이라고 정의내릴 수 있다.
# 400미터 달리기(400), 1500미터 달리기(1500)는 PC2와 강한 양의 부하량을 가지고 있다. PC2는 장거리 달리기와 같은 지구력과 연관있는 성분이라고 정의 내릴수 있다.
# 쇠공던지기(poid), 창던지기(jave), disq(원반던지기)는 PC2와 강한 음의 부하량을 가지고 있다.
# 하체운동과는 가장 반대되는 종목이기 때문에 PC2와 양의 관계를 보였던 하체 종목과는 반대되는 방향을 보이고 있다고 결론 내릴 수 있다.
