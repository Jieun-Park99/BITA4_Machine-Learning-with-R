states = as.data.frame(state.x77)
colnames(states)

# 1-1
fit = lm(Murder~., data=states)
fit2 = step(fit, direction="both")
fit2

# 1-2
# Population, Illiteracy, Life Exp, Frost, Area

# 1-3
# population이 한 단위 증가할 때 Murder가 1.780e-04배 증가한다.
# Area가 한 단위 증가할 때 Murder가 6.804e-06배 증가한다.


# 2-1
setwd('C:/Users/danan/Desktop/Bitamin/ML_R/reviews')
ex1 = read.csv('사례연구3.csv',header=T)
str(ex1)
#ex1['癤퓋ubject']
ex1$癤퓋ubject = factor(ex1$癤퓋ubject)
ex1$age = factor(ex1$age)
ex1$teaching = factor(ex1$teaching)
levels(ex1$teaching)

library(nnet)
ex1.multi = multinom(teaching~.,data=ex1)
ex1.sum = summary(ex1.multi)
ex1.sum
round(exp(ex1.sum$coefficients),3)
# 2-2
# science : science를 듣는 사람들이 토론식 수업보다 강의형 수업을 선택할 odds가 0.057배 높다.
# age11 : age11인 사람들이 토론식 수업보다 강의형 수업을 선택할 odds가 1.825배 높다.
# age13 : age13인 사람들이 토론식 수업보다 강의형 수업을 선택할 odds가 0.559배 높다.
# y = 0일때 discuss이고, y=1일때 lecture!!!

# 2-3
# F / 근거:상대적인 오즈의 차이를 해석하므로 절편은 해당 없음

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
m1 = read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
with(m1, table(ses,prog))
with(m1, do.call(rbind,tapply(write, prog, function(x) c(M=mean(x), SD= sd(x)))))

# 3-1
str(m1)
levels(m1$prog)
m1$prog2=relevel(m1$prog, ref="academic")
levels(m1$prog2)
m1.multi = multinom(prog2~ses+write,data=m1)
m1.sum = summary(m1.multi)
m1.sum

z = m1.sum$coefficients/m1.sum$standard.errors
p_value = round((1-pnorm(abs(z), 0, 1))*2,3)
p_value

# 3-2
round(exp(m1.sum$coefficients),3)
# 프로그램타입이 general인 학생들이 academic을 쓰는 학생들보다 ses가 middle일 odds가 0.587배높다.
# 프로그램타입이 general인 학생들이 academic을 쓰는 학생들보다 ses가 high일 odds가 0.313배높다.
# 프로그램타입이 general인 학생들이 academic을 쓰는 학생들보다 write score가 0.944배 높다.
# 프로그램타입이 vocation인 학생들이 academic을 쓰는 학생들보다 ses가 middle일 odds가 1.338배높다.
# 프로그램타입이 vocation인 학생들이 academic을 쓰는 학생들보다 ses가 high일 odds가 0.374배높다.
# 프로그램타입이 vocation인 학생들이 academic을 쓰는 학생들보다 write score가 0.893배 높다.

