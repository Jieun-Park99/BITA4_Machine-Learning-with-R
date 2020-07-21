#모든 코드 전에는 set.seed(300)을 공통적으로 해 줍니다. 
#데이터 Loan payments data는 부채에 관련된 데이터이고, 
#기한 내에 상환을 성공할 고객(Success)와 실패할 고객(Failure)을 종속변수로 간주했습니다.

#더 정확한 변수설명은 첨부파일에 올려놓았습니다.
library(dplyr)
library(rpart)
library(adabag)
setwd("C:/Users/danan/Desktop/Bitamin/ML_R/reviews")
loan<-read.csv("Loan payments data.csv")
str(loan)

#데이터 정리
loan <- loan %>% 
  # 이번 classification에 사용할 변수들만 선택, 추출
  select("loan_status", "Principal", "age", "education", "Gender") %>% 
  # target feature인 loan_status를 2개 범주로 변환 
  mutate(loan_status = factor(ifelse(loan_status == "PAIDOFF", "Success", "Failure")))

str(loan)

set.seed(300)
idx<-sample(1:nrow(loan),nrow(loan)*0.7)
tr_loan<-loan[idx,]
te_loan<-loan[-idx,]

## 1-1 mfinal=50, control은 깊이가 1이 되게끔 조정해서 Adaboosting을 해주세요
set.seed(300)
loan_ada = boosting(loan_status~., data=tr_loan, mfinal=50,
                    control=rpart.control(maxdepth = 1))
## 1-2 importance를 구해 어떤 변수가 가장 중요한 변수인지 확인하고, plot을 그려보세요.
loan_ada$importance
importanceplot(loan_ada)
## 2-1 mfinal=70일 때의 boosting 함수를 만들어 준 뒤, tr_loan, te_loan을 이용해서 과적합 여부를 판단하세요.
set.seed(300)
loan_ada2 = boosting(loan_status~., data=tr_loan, mfinal=70)
set.seed(300)
p_loan_ada2 = predict(loan_ada2, te_loan)
p_loan_ada2$error
set.seed(300)
tr_loan_ada2 = predict(loan_ada2, tr_loan)
tr_loan_ada2$error
# 과적합!

## 2-2 그렇게 생각한 이유를 서술하시오.
#error가 20정도 차이나는 것으로 보아 과적합 됐다고 볼 수 있다.

## 3번 2번에서 본인이 판단한 것에 기반해 최적의 mfinal을 찾아 보세요.(for loop 사용)
errors=c()
for (i in -5:5){
  set.seed(300)
  ada = boosting(loan_status~., data=tr_loan, mfinal=70+i)
  tr_ada = predict(ada, tr_loan)
  p_ada = predict(ada, te_loan)
  errors = c(errors,tr_ada$error-p_ada$error)
}
errors
which.max(errors)





