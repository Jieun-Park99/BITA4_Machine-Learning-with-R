### parallel 패키지
library(parallel)

# 코어 개수 획득
numCores = parallel::detectCores() -1

# 클러스터 초기화 -> 백엔드 결과 추가하기 (작업관리자)
myCluster = parallel::makeCluster(numCores)

# CPU 병렬처리
parallel::parLapply(cl=myCluster, X=2:4, fun=function(X) {2^X})

# 클러스터 중지
parallel::stopCluster(myCluster)


## 병렬처리실습
myCluster = parallel::makeCluster(numCores)
setwd("C:/Users/danan/Desktop/Bitamin/BITA4_Machine-Learning-with-R")
iseq = seq(1,10000,1)
parLapply(myCluster, iseq, function(y){
  write(y, "progress.txt", append=T)
})

# 클러스터 중지
parallel::stopCluster(myCluster)

### 변수스코프 
numCores = parallel::detectCores() - 1

# 클러스터 초기화
myCluster = parallel::makeCluster(numCores)

# 변수 등록
base = 2
parallel::clusterExport(myCluster, "base") # base 인식!

# CPU 병렬처리
parallel::parLapply(cl=myCluster,
                    X = 2:4,
                    fun=function(X){
                      base^X
                    })
# 클러스터 중지
parallel::stopCluster(myCluster)

### foreach 패키지
library(foreach)
library(doParallel)

# 코어 개수 획득
numCores = parallel::detectCores() -1

# 클러스터 초기화
myCluster = parallel::makeCluster(numCores)
doParallel::registerDoParallel(myCluster)

# 변수 등록, 안해도 상관없음
base=2
parallel::clusterExport(myCluster,"base")

# CPU 병렬처리, C는 cbind 느낌
foreach::foreach(exponent = 2:4, combine=c) %dopar%{
  base^exponent
}

# 클러스터 중지
parallel::stopCluster(myCluster)


## foreach() 함수를 별도의 외부 함수로 정의할 경우 오류가 나므로 외부변수를 
# 사용하는데 있어 불편함을 해소하기 위하여 export 옵션 제공
test = function(exponent){
  foreach::foreach(exponent = 2:4,
                   .combine = c,
                   .export = "base") %dopar% {
                     base^exponent
                   }
}

test()

# 코어 개수 획득
numCores = parallel::detectCores() -1

# 클러스터 초기화
myCluster = parallel::makeCluster(numCores)
doParallel::registerDoParallel(myCluster)

library(caret)
set.seed(1234)
folds = createFolds(iris$Sepal.Length, k=3)

td_tmp = foreach::foreach(k=1:3,
                          .combine = rbind,
                          .packages = c("dplyr","broom","caret"),
                          .inorder = TRUE) %dopar% {
                            tmp = iris[-unlist(folds[k]),] %>% group_by(Species) %>%
                              do(fit = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=.))
                              tidy(tmp,fit)
                          }
# 클러스터 중지
parallel::stopCluster(myCluster)

#########
# 코어 개수 획득
numCores = parallel::detectCores() -1

# 클러스터 초기화
myCluster = parallel::makeCluster(numCores, type = "PSOCK")

# CPU 병렬처리
foreach(X=list(1,2,"a")) %dopar% {
  tryCatch({
    c(1/X,X,2^X)
  }, error=function(e) {
    return(paste0("The variable '",X,"'"," caused the error: ",e,"'"))
  })
}

# 클러스터 중지
parallel::stopCluster(myCluster)

system.time({for(i in 1:10000){
  i+5
}})

n_core = detectCores()
cl = makeCluster(n_core-1)
registerDoParallel(cl)
system.time(
  {foreach(i = 1:10000) %dopar%{
    i+5
  }}
)

################ h2o 실습 - 데이터 준비 ############
library(dplyr)
library(caret)

flights_data = readRDS("C:/Users/danan/Desktop/Bitamin/BITA4_Machine-Learning-with-R/flights.RDS")
head(flights_data)
str(flights_data)

flights_data$target = ifelse((is.na(flights_data$dep_delay) | (flights_data$dep_delay<=30 & flights_data$dep_delay >= -30)) &
                               (is.na(flights_data$arr_delay) | (flights_data$arr_delay <=30 & flights_data$arr_delay >= -30)), "normal","delay")

table(flights_data$target)

# 모델링을 위해 일부 변수만 사용 및 categorical 변수 factor로 변경
final_data = flights_data %>%  select("month","carrier","flight","dest","air_time","distance","target")
str(final_data)

final_data$carrier = as.factor(final_data$carrier)
final_data$dest = as.factor(final_data$dest)
final_data$target = as.factor(final_data$target)

## train, test 나누기
set.seed(1234)
train_idx = createDataPartition(final_data$target, p=0.7, list=F)
train = final_data[train_idx, ]
test = final_data[-train_idx, ]

library(h2o)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-13.0.1")
h2o.init(nthreads = 15, max_mem_size = "10g")
# h2o::h2o.shutdown(prompt = FALSE)

train_data_h2o = as.h2o(train, destination_frame = "train_data_h2o")
test_data_h2o = as.h2o(test, destination_frame = "test_data_h2o")

target = "target"
features = names(train)[!names(train) %in% target]
target ; features

rf_model = h2o.randomForest(x = features, y=target, training_frame = train_data_h2o,
                            model_id = "rf_model", ntrees = 500, seed=1234, mtries = floor(ncol(train) / 3), verbose=F)

rf_model
test_predict = h2o.predict(rf_model, newdata = test_data_h2o)
test_predict

h2o.confusionMatrix(rf_model.newdata= test_data_h2o)
h2o.confusionMatrix(rf_model, newdata=test_data_h2o, metrics="accuracy")
h2o.confusionMatrix(rf_model, newdata= test_data_h2o, metrics = "accuracy", thresholds=0.5)

plot(rf_model)

h2o.varimp_plot(rf_model, num_of_features = 6)

######################## R에서 메일 보내기 Naver ###########################
library(mailR)
send.mail(from="dananine@naver.com", # 보내는 사람(@naver.com)
          to="dananine@naver.com", # 받는 사람
          subject="R 사용이 끝났습니다", # 메일 제목
          body="끝났다고~~", # 메일 내용
          html=T,
          
          smtp = list(host.name = "smtp.naver.com",#메일서버 정보 (네이버)
                      port=465, # 고정값
                      user.name="dananine", #네이버아이디
                      passwd="wldmsdktkfkdgo03", #네이버 비밀번호
                      ssl=T),
          
          encoding="utf-8", #한글 사용시 필요(고정값)
          authenticate=T) #고정값

###### Plotly 기초 #######
library(plotly)
library(dplyr)
library(ggplot2)

#plot_ly(
#  data = 
#    x =x,
#  y = y1,
#  type="",
#  mo
#)

### 1. interactive plot ###
flights_data %>% 
  arrange(dep_dt) %>% 
  filter(month==1) %>% 
  plot_ly(
    x = ~dep_dt,
    y = ~dep_delay,
    type="scatter"
  )

##################################################################


flights_data
library(dplyr)
library(ggplot2)
flights_data %>% 
  na.omit() %>% 
  mutate(delay = as.factor(ifelse(dep_delay>30,1,0))) %>% 
  arrange(dep_dt) %>%
  filter(month==1) %>% 
  ggplot(aes(x=dep_dt,y=dep_delay,
             shape= delay, col=delay)) +
  geom_point()+
  theme_bw()



flights_data %>% 
  arrange(dep_dt) %>%
  filter(month==1) %>% 
  plot_ly(
    x = ~dep_dt,
    y = ~dep_delay,
    color = ~carrier,
    type="scatter"
  )
