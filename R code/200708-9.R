# 200708-9

getwd()
setwd("C:/Users/Admin/Desktop/공공 빅데이터 청년 인턴십/R")

#####################################################################################
# 미니 프로젝트
#####################################################################################

PSDS_PATh = file.path(('C:/Users/Admin/Desktop/공공 빅데이터 청년 인턴십/R'))
loan_data = read.csv(file.path(PSDS_PATh, 'data', 'loan_data.csv'), stringsAsFactors=T)
#loan_data = read.csv(file.path(PSDS_PATh, 'data', 'loan_data_pre.csv'), stringsAsFactors=T)

dim(loan_data)
str(loan_data)
head(loan_data)

loan_data = loan_data[, c('loan_amnt','term','annual_inc','purpose','home_ownership','grade','outcome','emp_length')]
colnames(loan_data) = c('대출금액', '대출기간', '연간소득', '대출목적', '주택분류', '신용지표','파산여부','근속기간')

# 파산여부 데이터 변환(영문 -> 한글)
loan_data$파산여부 = gsub('default', '파산', loan_data$파산여부)
loan_data$파산여부 = gsub('paid off', '상환', loan_data$파산여부)

# 주택분류 데이터 변환(영문 -> 한글)
loan_data$주택분류 = gsub('MORTGAGE', '담보대출', loan_data$주택분류)
loan_data$주택분류 = gsub('RENT', '임대', loan_data$주택분류)
loan_data$주택분류 = gsub('OWN', '자가', loan_data$주택분류)
loan_data$주택분류 = gsub('OTHER', '기타', loan_data$주택분류)

# 대출기간 데이터 변환(영문 -> 한글)
loan_data$대출기간 = gsub('36 months', "단기", loan_data$대출기간)
loan_data$대출기간 = gsub('60 months', "장기", loan_data$대출기간)

# 대출목적 데이터 변환(영문 -> 한글)
loan_data$대출목적 = gsub('credit_card', '신용카드', loan_data$대출목적)
loan_data$대출목적 = gsub('car', '자동차', loan_data$대출목적)
loan_data$대출목적 = gsub('small_business', '사업', loan_data$대출목적)
loan_data$대출목적 = gsub('other', '기타', loan_data$대출목적)
loan_data$대출목적 = gsub('debt_consolidation', '채무통합', loan_data$대출목적)
loan_data$대출목적 = gsub('major_purchase', '생필품구매', loan_data$대출목적)
loan_data$대출목적 = gsub('home_improvement', '주택보수', loan_data$대출목적)
loan_data$대출목적 = gsub('moving', '이사', loan_data$대출목적)
loan_data$대출목적 = gsub('house', '주택', loan_data$대출목적)
loan_data$대출목적 = gsub('medical', '의료', loan_data$대출목적)
loan_data$대출목적 = gsub('wedding', '결혼', loan_data$대출목적)
loan_data$대출목적 = gsub('vacation', '휴가', loan_data$대출목적)

#####################################################################################
# EDA
# 빈도표 생성
library(descr)
### 목적 & 파산 여부
pur_out = CrossTable(loan_data$대출목적, loan_data$파산여부, prop.c = F, prop.chisq = F, prop.t = F)
pur_out

### 목적 & 대출기간
pur_term= CrossTable(loan_data$대출목적, loan_data$대출기간, prop.c = F, prop.chisq = F, prop.t = F)
pur_term

### 주택분류 & 파산여부
home_out = CrossTable(loan_data$주택분류, loan_data$파산여부, prop.c = F, prop.chisq = F, prop.t = F)
home_out

#install.packages('lmPerm')
library(lmPerm)
### ANOVA(대출목적에 따른 신용지표  평균차이 검정)
summary(aov(신용지표 ~ 대출목적, data = loan_data))

### 파산여부에 따른 신용지표 평균 차이 검정)
t.test(신용지표 ~ 파산여부,data = loan_data, alternative = 'greater')

# 시각화
library(ggplot2)
library(lattice)

### 대출목적별 파산 여부 비율 확인
ggplot(loan_data, aes(x = 파산여부, fill = 파산여부)) + 
  geom_bar() +
  facet_grid(. ~ 대출목적) +
  ggtitle('대출 목적별 파산 여부 비율') +
  xlab('파산 여부')+
  ylab('빈도')

library(dplyr)

d3 = loan_data %>% group_by(목적 = 대출목적, 파산여부 = 파산여부) %>% summarise(평균연간소득 = mean(연간소득),
                                                                        평균대출금액 = mean(대출금액),
                                                                        평균신용지표 = mean(신용지표))
### 평균신용지표 평균연간소득 대출금액 파산여부 버블차트
ggplot(d3, aes(x = 평균신용지표, y = 평균연간소득, fill = 파산여부)) +
  geom_point(aes(size = 평균대출금액), shape = 21, colour = 'black',
             alpha = .6)+
  geom_text(aes(y = as.numeric(평균연간소득) - 1000, x = as.numeric(평균신용지표) + .1), label = d2$목적,
            size = 5, colour = 'grey20')

## 대출목적 파이차트
pie(table(loan_data$대출목적), cex = 1)

### 파산여부별 대출금액 비교(파란점 = 평균 / 빨간점 = 이상치)
ggplot(loan_data, aes(x = 파산여부, y = 대출금액))+
  geom_boxplot(width = .6, outlier.size = 3,
               outlier.shape = 14, outlier.colour = 'red')+
  stat_summary(fun = 'mean', geom = 'point',
               shape = 21, size = 3, fill = 'blue')

### 대출목적 TOP 5 차트
t = sort(table(loan_data$대출목적), decreasing = T)
colors <- c()

for ( i in 1:length(t[1:5])){
  if(t[i] >= 10000){
    colors <- c(colors,"red")
  } else if(t[i] >=5000){
    colors <- c(colors,"yellow")
  } else if(t[i] >=2000){
    colors <- c(colors,"blue")
  } else {
    colors <- c(colors, "green")
  }
}

b_chart <- barplot(t[1:5], main="대출목적 TOP 5", 
                   ylab="대출건수", names.arg=names(t[1:5]),
                   col=colors, las=2, ylim=c(0,30000))
text(x=b_chart, y=t[1:5]+ 2000, labels=paste(t[1:5],"건"), col="black", cex= 1.5)
pct <- round(t[1:5]/sum(t[1:5])*100,1)
text(x=b_chart, y=t[1:5]+500, labels=paste(  "(",pct,"%)"  ),
     
     col="black", cex= 1)

### 대출목적별 평균 대출금액
t = loan_data %>% group_by(목적 = 대출목적) %>% summarise(평균연간소득 = mean(연간소득),
                                                          평균대출금액 = mean(대출금액),
                                                          평균신용지표 = mean(신용지표))
x = c("결혼", "기타", "사업",
      "생필품구매", "신용카드", "의료",
      "이사", "자동차", "주택",
      "주택보수", "채무통합", "휴가" )
n = c(68108.50, 63336.03, 78709.76, 67871.93,
      68138.76, 66911.24, 62587.83, 61969.16,
      74164.99, 83911.96, 67050.44,59739.87)
t = sort(xtabs(n ~ x), decreasing = T)
t

colors <- c()

for ( i in 1:length(t[1:5])){
  if(t[i] >= 80000){
    colors <- c(colors,"red")
  } else if(t[i] >=75000){
    colors <- c(colors,"yellow")
  } else if(t[i] >=70000){
    colors <- c(colors,"green")
  } else {
    colors <- c(colors, "blue")
  }
}
b_chart <- barplot(t[1:5], main="대출금액 TOP 5", 
                   ylab="대출금액", names.arg=names(t[1:5]),
                   col=colors, las=2, ylim=c(0,90000))

library(corrplot)
#install.packages('klaR')
library(klaR)
#install.packages('caret')
library(caret)

# 시드 고정
set.seed(1234)

# model1(xgboost)
### train, test = 8:2
sample_n = sample(1:nrow(loan_data), size=round(0.2*nrow(loan_data)))
train = loan_data[-sample_n, ]; dim(train) # (36274, 8)
test = loan_data[sample_n, ]; dim(test) # (9068, 8)

# 더미화 패키지 다운로드
install.packages('fastDummies')
library(fastDummies)

train = cbind(dummy_cols(train[,-7])[,c(-2,-4,-5)], '파산여부' = train[,7])
test = cbind(dummy_cols(test[,-7])[,c(-2,-4,-5)], '파산여부' = test[,7])

#install.packages('xgboost')
library(xgboost)
train$파산여부 = gsub('파산', as.numeric(1), train$파산여부)
train$파산여부 = gsub('상환', as.numeric(0), train$파산여부)
test$파산여부 = gsub('파산', as.numeric(1), test$파산여부)
test$파산여부 = gsub('상환', as.numeric(0), test$파산여부)

xgb = xgboost(data = data.matrix(train[,-23]), label = train[,23],
              objective = 'binary:logistic',
              params = list(subsample = .63, eta = .1), nrounds = 10)
pred = predict(xgb, newdata = data.matrix(test[,-23]))

# threshold 0.5 설정
xgb_df = cbind(test, pred_default = pred > .5, prob_default = pred)

xgb_df$pred_default = gsub(TRUE, 1, xgb_df$pred_default)
xgb_df$pred_default = gsub(FALSE, 0, xgb_df$pred_default)

# 전체 행중 예측값과 실제값 같은 행 개수 = 정확도
nrow(subset(xgb_df, pred_default == 파산여부)) / nrow(xgb_df)

# model2(naivebayes, glm)
PSDS_PATh = file.path(('C:/Users/Admin/Desktop/怨듦났 鍮낅뜲?씠?꽣 泥??뀈 ?씤?꽩?떗/R'))
loan_data = read.csv(file.path(PSDS_PATh, 'data', 'loan_data.csv'), stringsAsFactors=T)

loan_data$outcome = gsub('default', '파산', loan_data$outcome)
loan_data$outcome = gsub('paid off', '상환', loan_data$outcome)

loan_data$home_ownership = gsub('MORTGAGE', '담보대출', loan_data$home_ownership)
loan_data$home_ownership = gsub('RENT', '임대', loan_data$home_ownership)
loan_data$home_ownership = gsub('OWN', '자가', loan_data$home_ownership)
loan_data$home_ownership = gsub('OTHER', '기타', loan_data$home_ownership)

loan_data$term = gsub('36 months', "단기", loan_data$term)
loan_data$term = gsub('60 months', "장기", loan_data$term)

# 분석 준비
set.seed(1234)
sample_n = sample(1:nrow(loan_data), size=round(0.2*nrow(loan_data)))
train = loan_data[-sample_n, ]; dim(train) # (36274, 21)
test = loan_data[sample_n, ]; dim(test) # (9068, 21)

# NaiveBayes :  범주형 변수만 사용
naive_model = NaiveBayes(outcome ~ term + purpose_ + home_ + emp_len_, data=na.omit(train))
naive_model$tables
pred = predict(naive_model, test); head(pred$class, 10)

confusionMatrix(test$outcome, pred$class) # 정확도 : 59%

# 로지스틱 회귀분석
# 수치형 변수 비교
logis_nume_full = glm(outcome ~ loan_amnt + annual_inc + dti + payment_inc_ratio + 
                        revol_bal + revol_util + delinq_2yrs_zero + pub_rec_zero + 
                        open_acc + grade + emp_length + borrower_score, 
                      data=na.omit(train), family='binomial')
summary(logis_nume_full) # loan_amnt, delinq_2yrs_zero, open_acc, emp_length > 4개 삭제

# 단계적 회귀로 추가 선택 >>> 에러 해결 필요!
logis_nume = glm(outcome ~ annual_inc + dti + payment_inc_ratio + revol_bal + 
                   revol_util + pub_rec_zero + grade + borrower_score,
                 data=na.omit(train), family='binomial')
stepAIC(logis_nume, direction = 'both')

# 책에 나온 이상적인 모형
logis_model = glm(outcome ~ payment_inc_ratio + purpose_ + home_ + emp_len_ + borrower_score, 
                  data=na.omit(train), family='binomial')
summary(logis_model)

pred = predict(logis_model, test, type="response")

result = rep('default', nrow(test))
result[pred>0.5] = 'paid off'
table(test$outcome, result)
mean(test$outcome == result) # 정확도

# 로지스틱 10-fold 교차검증
cv_list = createFolds(train$outcome, k=10)

accu = rep(0, length(cv_list))
for (i in 1:length(cv_list)) {
  idx = cv_list[[i]]
  cv_train = train[idx, ]
  cv_test = train[-idx, ]
  
  logis_model = glm(outcome ~ payment_inc_ratio + purpose_ + home_ + emp_len_ + borrower_score, 
                    data=na.omit(cv_train), family='binomial'(link='logit'))
  pred = predict(logis_model, cv_test, type="response")
  result = rep('default', nrow(cv_test))
  result[pred>0.5] = 'paid off'
  
  accu[i] = mean(cv_test$outcome == result)
}
accu; mean(accu)



