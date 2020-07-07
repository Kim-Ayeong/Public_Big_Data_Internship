# 200706

getwd()
setwd("C:/Users/Admin/Desktop/공공 빅데이터 청년 인턴십/R")

#####################################################################################
# <데이터형과 연산>
#####################################################################################

# 변수
x = 1
y = 2
x+y

x = x+1; x
(x = x+1) # 동일

# 데이터형
x = 5
y = 2
x/y 

xi = 1+2i
yi = 1-2i
xi+yi # 숫자형

(text = 'hello world') # 문자형

(blood.type = factor(c('A', 'B', '0', 'AB'))) # 범주형

T
F # 논리형

xinf = Inf
yinf = Inf
xinf/yinf # NaN

# 데이터형
class(x)
typeof(x)

x = 1
is.numeric(x) # true
is.integer(x) # false

x = as.integer(1) 
is.integer(x) # true

x = 1L # 정수형 고정
is.numeric(x) # true
is.integer(x) # true

5+2
5-2
5*2
5/2
5%%2 # 나머지
5%/%2 # 몫

5<5
5<=5
5>5
5>=5
5==5
5!=5
5!=TRUE
TRUE|TRUE
TRUE&FALSE
isTRUE(TRUE)

##########################################

# 벡터
1:7
7:1
c(1, 2, 3, c(4:6)) # 연결
y = c()
(y = c(y, c(1:3))) # 빈 벡터에 연결

seq(from=1, to=10, by=2)
seq(0, 1, length.out=11) # 11개 요소

rep(c(1:3), times=2) # 2번 반복
rep(c(1:3), each=2) # 2번씩 반복

x = c(2, 4, 6, 8, 10)
length(x)
x[1] # R은 1부터 시작
x[1, 2, 3] # error!!!
x[c(1, 2, 3)]
x[-c(1, 2, 3)]
x[c(1:3)]

# 벡터 연산
x = c(1, 2, 3, 4)
y = c(5, 6)
z = c(5, 6, 7)

x+y # 작은 벡터의 정수배 연산
x+z # error!!!

x = 1:5
x>3
all(x>3) # false
any(x>3) # true

head(x, 3) # default = 6
tail(x, 3)

y = 3:6
union(x, y)
intersect(x, y)
setdiff(x, y)
setequal(x, y) #원소 동일

########################################

# 행렬 : 데이터형이 같아야함
x = array(1:5, c(2, 4))
x[1, ]
x[, 2]

dimnamex = list(c('1st', '2nd'), c('일', '이', '삼', '사'))
(x = array(1:5, c(2, 4), dimnames=dimnamex))
x['1st', ]
x[, '사']

x = 1:12
matrix(x, nrow=3)
matrix(x, nrow=3, byrow=T) # 행 우선 배치

v1 = c(1,2,3)
v2 = c(4,5,6)
cbind(v1, v2) # 열 단위
rbind(v1, v2) # 행 단위

x = array(1:4, dim=c(2, 2))
y = array(5:8, dim=c(2, 2))
x+y
x-y
x*y # 열 단위 곱셈

x = array(1:12, c(3, 4))
apply(x, 1, mean) # 행
apply(x, 2, mean) # 열
dim(x)

sample(x) # x에서 하나씩 샘플 추출
sample(10)
sample(10, 2) # 샘플 2개 추출

######################################  

# 데이터 프레임 : 데이터형이 달라도 가능, 길이는 같아야함
name = c('하나', '둘', '셋')
age = c(22,  20, 25)
gender = factor(c('M', 'F', 'M'))
blood = factor(c('A', 'O', 'B'))
(patients = data.frame(name, age, gender, blood))

test = data.frame()
test = edit(test) # 직접 입력

patients$name
patients[1, ]
patients[, 2]
patients[3, 1]

patients[patients$name=='하나', ]
patients[patients$name=='하나', 'name']
patients[patients$name=='하나', c('name', 'age')]

head(cars)
attach(cars) # search()로 확인
speed
detach(cars)
speed

subset(cars, speed>20, select=c(dist))
subset(cars, speed>20, select=-c(dist))

###########################################

# 리스트 : 데이터형이 달라도 가능, 길이가 달라도 가능
(no.patients = data.frame(day=c(1:6), no=c(50, 60, 66, 62, 76, 58)))

(listpatients = list(patients, no.patients))
(listpatients = list(patients=patients, no.patients=no.patients)) # 요소명 지정

listpatients$patients
listpatients[[1]] # 인덱스로 접근
listpatients[['patients']] # 요소명으로 접근

lapply(listpatients$no.patients, mean)
typeof(lapply(listpatients$no.patients, mean)) # list로 반환

sapply(listpatients$no.patients, mean)
typeof(sapply(listpatients$no.patients, mean)) # double(벡터/행렬)로 반환

########################################################################################
# <파일 읽고 쓰기>
########################################################################################

rm(list=ls())

# txt 파일
(students = read.table("./data/students.txt", header=T))
str(students) # factor

(students = read.table("./data/students.txt", header=T, as.is=T)) # 그대로 인식
str(students) # character, factor로 변환X

# csv 파일
(students = read.csv("./data/students.csv"))
str(students) # factor
students$name = as.character(students$name)

(students = read.csv("./data/students.csv", stringsAsFactors=F)) # 그대로 인식
str(students) # character

# 저장
write.table(students, file="./data/output.txt")
write.table(students, file="./data/output.txt", quote=F) # 문자에 ""를 붙이지 않음

#######################################

# 데이터 정제를 위한 조건문
test = c(15, 20, 30, NA, 45)
test[test%%3 != 0]
test[is.na(test)]
test[test%%2 == 0 & !is.na(test)]

name = c('하나', '둘', '셋')
age = c(22,  20, 25)
gender = factor(c('M', 'F', 'M'))
blood = factor(c('A', 'O', 'B'))
(patients = data.frame(name, age, gender, blood))
patients[patients$age < 25, patients$gender == 'F']

x = 5
if (x%%2 != 0) {
  print('홀수')
} else if (x%%2 == 0){
  print('짝수')
} else {
  print('0')
}

x = c(-5:5)
options(digits=3) # 출력 숫자 유효자리
sqrt(x) # NaN 출력
sqrt(ifelse (x>=0, x, NA)) # NaN 대신 NA 출력

i = 1
repeat {
  if (i>10) {
    break
  } else {
    print(i)
    i = i+1
  }
}

i = 1
while (i<=10) {
  print(i)
  i = i+1
}

for (i in 1:10) {
  print(i)
}

fact = function(x) {
  fa = 1
  while (x>1) {
    fa = fa*x
    x = x-1
  }
  return(fa)
}
fact(5)

######################################

# 예제1 : 결측값 처리
str(airquality)
head(airquality)
table(is.na(airquality))

# 방법1
air_narm1 = airquality[!is.na(airquality$Ozone), ]
mean(air_narm1$Ozone)

# 방법2
air_narm2 = na.omit(airquality); mean(air_narm2$Ozone)

# 방법3
mean(airquality$Ozone, na.rm=T)

# 예제2 : 이상값 처리
boxplot(airquality)

# 방법1
patients_outrm1 = patients[(patients$gender == 'M' | patients$gender == 'F') & 
                             (patients$blood == 'A' | patients$blood == 'B' | 
                                patients$blood == 'O' | patients$blood == 'AB'), ]
# 방법2 < 숫자로 재범주화한 후
patients$gender = ifelse((patients$gender<1 | patients$gender>2), NA, patients$gender)
patients$blood = ifelse((patients$blood<1 | patients$blood>2), NA, patients$blood)
patients[!is.na(patients$gender) & !is.na(patients$blood), ] # 결측값 제거

# 방법3
boxplot(airquality)
boxplot(airquality[, 1])$stats # 통계량 확인

air = airquality # 복제
air$Ozone = ifelse(air$Ozone<1 | air$Ozone>122, NA, air$Ozone) # 직접 값 입력
air_narm = air[!is.na(air$Ozone), ]
boxplot(air_narm$Ozone)$stats

#####################################################################################
# <데이터 가공>
#####################################################################################

# install.packages("gapminder")
library(gapminder)
# install.packages("dplyr")
library(dplyr)
# install.packages("ggplot2")
library(ggplot2)

glimpse(gapminder) # 요약 통계량, str과 유사

gapminder[, c('country', 'lifeExp')]
gapminder[gapminder$country == 'Croatia' & gapminder$year > 1990, c('lifeExp', 'pop')]

# dplyr
select(gapminder, country, year, lifeExp) # 열 추출
filter(gapminder, country == "Croatia") # 행 추출
summarise(gapminder, pop_avg=mean(pop)) # 요약
summarise(group_by(gapminder, continent, country), pop_avg=mean(pop)) # 그룹별 요약

# 일련의 과정 연결, 위와 동일
gapminder %>%
  group_by(continent, country) %>%
  summarise(pop_avg=mean(pop))

#######################################

# 데이터 가공 예제 : 방대한 데이터 요약(캐글)
avocado = read.csv("./data/avocado.csv", header=TRUE, sep=",")
str(avocado)

(x_avg = avocado %>%
  group_by(region, year) %>%
  summarise(V_avg=mean(Total.Volume), P_avg=mean(AveragePrice)))

# 세분화
(x_avg = avocado %>%
    group_by(region, year, type) %>%
    summarise(V_avg=mean(Total.Volume), P_avg=mean(AveragePrice)))

(x_avg %>%
  filter(region!='TotalUS') %>%
  ggplot(aes(year, V_avg, col=type)) + geom_line() + facet_wrap(~region))

arrange(x_avg, desc(V_avg)) # 정렬

x_avg1 = x_avg %>% filter(region!='TotalUS')
x_avg1[x_avg1$V_avg == max(x_avg1$V_avg), ]

# install.packages("lubridate")
library(lubridate)

(x_avg = avocado %>%
    group_by(region, year, month(Date), type) %>%
    summarise(V_avg=mean(Total.Volume), P_avg=mean(AveragePrice)))

#########################################

# 데이터 가공 예제 : 모델링을 위한 가공(UCI)
wine = read.table('./data/wine.data', header=TRUE, sep=',')
head(wine)

(n = readLines('./data/wine.name.txt'))
(names(wine)[2:14] = substr(n, 4, nchar(n)))

train_set = sample_frac(wine, 0.6)
str(train_set)

test_set = setdiff(wine, train_set)
str(test_set)

#########################################

# 데이터 가공 예제 : 데이터 구조 변경(gapminder)
elec_gen = read.csv('./data/electricity_generation_per_person.csv', header=TRUE, sep=',')
names(elec_gen)
(names(elec_gen) = substr(names(elec_gen), 2, nchar(names(elec_gen)))) # x 제거


elec_use = read.csv('./data/electricity_generation_per_person.csv', header=TRUE, sep=',')
(names(elec_use[2:33]) = substr(names(elec_use[2:33]), 2, nchar(names(elec_use[2:33]))))

# install.packages("tidyr")
library(tidyr)

elec_gen_df = gather(elec_gen, country, key='year', value='ElecGeneration')
names(elec_gen) = c('country', 'year', 'ElecGeneration')
elec_use_df = gather(elec_use, country, key='year', value='ElecUse')
(elec_gen_use = merge(elec_gen_df, elec_use_df))





  