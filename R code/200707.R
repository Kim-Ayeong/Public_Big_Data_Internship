# 200707

getwd()
setwd("C:/Users/Admin/Desktop/공공 빅데이터 청년 인턴십/R")

#####################################################################################
# 탐색적 데이터 분석
#####################################################################################

# install.packages("klaR")
library(dplyr)
library(ggplot2)
library(corrplot)
library(descr)
library(klaR)

PSDS_PATh = file.path(('C:/Users/Admin/Desktop/공공 빅데이터 청년 인턴십/R'))
state = read.csv(file.path(PSDS_PATh, 'data', 'state.csv'))

head(state)
str(state)

# 데이터 분포 탐색
mean(state[['Population']]) # 평균
mean(state[['Population']], trim=0.1) # 절사평균
median(state[['Population']]) # 중앙값
weighted.mean(state[['Murder.Rate']], w=state[['Population']]) # 가중평균

var(state[['Population']]) # 분산
sd(state[['Population']]) # 표준편차
IQR(state[['Population']]) # 사분위수 범위
mad(state[['Population']]) # 중위절대편차

quantile(state[['Population']], p=c(0.05, 0.25, 0.5, 0.75, 0.95)) # 분위수

boxplot(state[['Population']]/1000000, ylab="Population (millions)")

(breaks = seq(from=min(state[['Population']]), to=max(state[['Population']]), length=11))
pop_freq = cut(state[['Population']], breaks=breaks, right=TRUE, include.lowest=TRUE)
table(pop_freq) # 도수분포표

hist(state[['Population']], breaks=breaks) # 히스토그램

dfw = read.csv(file.path(PSDS_PATh, 'data', 'dfw_airline.csv'))
barplot(as.matrix(dfw)/6, cex.axi=0.8, cex.names=0.7) # 막대 그래프

# 상관관계
sp500_px =  read.csv(file.path(PSDS_PATh, 'data', 'sp500_px.csv'), stringsAsFactors=FALSE)
head(sp500_px)
sp500_sym =  read.csv(file.path(PSDS_PATh, 'data', 'sp500_sym.csv'), stringsAsFactors=FALSE)
head(sp500_sym)

etfs = sp500_px[row.names(sp500_px) > '2012-07-01',
                sp500_sym[sp500_sym$sector == 'etf', 'symbol']]

# 상관관계 행렬
telecom = sp500_px[, sp500_sym[sp500_sym$sector == 'telecommunications_services', 'symbol']]
telecom = telecom[row.names(telecom) > '2012-07-01', ]

telecom_cor = cor(telecom)
telecom_cor

corrplot(cor(etfs), method='ellipse')
corrplot(cor(etfs), method='square')
corrplot(cor(etfs), method='shade')
corrplot(cor(etfs), method='pie')

# 산점도
plot(telecom$T, telecom$VZ, xlab='T', ylab='VZ')

##########################################################################################
# 데이터와 표본 분포
##########################################################################################

loans_income = read.csv(file.path(PSDS_PATh, 'data', 'loans_income.csv'))[, 1]

# 단순랜덤표본
samp_data = data.frame(income=sample(loans_income, 1000), 
                       type='data_dist')

# 5개씩 표본 추출 후 평균
samp_data_5 = data.frame(
  income = tapply(sample(loans_income, 1000*5),
                  rep(1:1000, rep(5, 1000)), FUN=mean), 
  type='mean_of_5'
  ) # 함수를 적용할 요인 수준 = (1,1,1,1,1,2,...999,1000,1000,1000,1000,1000)로 구분(총 5000행)

# 20개씩 표본 추출 후 평균
samp_data_20 = data.frame(
  income = tapply(sample(loans_income, 1000*20),
                  rep(1:1000, rep(20, 1000)), FUN=mean), 
  type='mean_of_20'
  )

income = rbind(samp_data, samp_data_5, samp_data_20)
income$type = factor(income$type, levels=c('data_dist', 'mean_of_5', 'mean_of_20'),
                     labels=c('Data', 'Mean of 5', 'Mean of 20'))
head(income)
str(income)

ggplot(income, aes(x=income)) + geom_histogram(bins=40) + facet_grid(type ~ .) # 중심극한정리

# 부트스트랩
library(boot)

stat_fun = function(x, idx) median(x[idx])
(boot_obj = boot(loans_income, R=1000, statistic=stat_fun))

#########################################################################################
# 통계적 실험과 유의성 검정
##########################################################################################

# t 검정 : 두 집단의 평균 차이 검정
session_times = read.csv(file.path(PSDS_PATh, 'data', 'web_page_data.csv'))
head(session_times)
str(session_times) # 범주형(2수준), 수치형

t.test(Time ~ Page, data=session_times, alternative='less') # 일원 t검정

# 분산분석(ANOVA) : 여러 집단의 분산 차이 검정
four_sessions = read.csv(file.path(PSDS_PATh, 'data', 'four_sessions.csv'))
head(four_sessions)
str(four_sessions) # 범주형(4수준), 수치형

summary(aov(Time ~ Page, data=four_sessions)) # 일원 분산분석

# f 검정 : 두 집단의 분산 차이 검정
var.test(Time ~ Page, data=session_times, alternative='less')

# 교차분석(카이제곱검정) : 범주형 변수 간 독립성 검정
clicks = read.csv(file.path(PSDS_PATh, 'data', 'click_rates.csv'))
head(clicks)
str(clicks) # 범주형(3수준), 범주형(2수준)

chisq.test(clicks$Headline, clicks$Click, simulate.p.value=TRUE)

# z 검정 : 두 집단의 모비율 차이 검정
dnorm(1.645, mean=0, sd=1) # 0.10, density, X=1.645에 해당하는 확률밀도 함수값
pnorm(1.645, mean=0, sd=1) # 0.95, probability, 누적확률값
qnorm(0.95, mean=0, sd=1) # 1.645, quantile, 누적확률에 해당하는 분위수
rnorm(10) # random, 난수 생성

smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )
prop.test(smokers, patients)

prop.test(x=95, n=160, p=0.5, alternative='two.sided', conf.level=0.95)

