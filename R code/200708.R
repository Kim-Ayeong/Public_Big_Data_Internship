# 200708

getwd()
setwd("C:/Users/Admin/Desktop/공공 빅데이터 청년 인턴십/R")

#####################################################################################
# 미니 프로젝트
#####################################################################################

library(dplyr)
library(ggplot2)
library(corrplot)
library(descr)
library(klaR)

PSDS_PATh = file.path(('C:/Users/Admin/Desktop/공공 빅데이터 청년 인턴십/R'))
loan_data = read.csv(file.path(PSDS_PATh, 'data', 'loan_data.csv'), stringsAsFactors=F)

head(loan_data)
str(loan_data)
dim(loan_data)

# 시각화
ggplot(loan_data, aes(x=term)) +
  geom_bar(fill='white', colour='black') + ggtitle("Bar Chart of Frequency by Term") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1)) 

ggplot(loan_data, aes(x=purpose)) +
  geom_bar(fill='white', colour='black') + ggtitle("Bar Chart of Frequency by Term") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1)) 

boxplot(loan_data[c('open_acc','grade','borrower_score')]/10000 ) # 'open_acc','grade','borrower_score'

# EDA
loan_data$outcome = gsub('default', '파산', loan_data$outcome)
loan_data$outcome = gsub('paid off', '상환', loan_data$outcome)

loan_data$home_ownership = gsub('MORTGAGE', '담보대출', loan_data$home_ownership)
loan_data$home_ownership = gsub('RENT', '임대', loan_data$home_ownership)
loan_data$home_ownership = gsub('OWN', '자가', loan_data$home_ownership)
loan_data$home_ownership = gsub('OTHER', '기타', loan_data$home_ownership)

loan_data$term = gsub('36 months', "단기", loan_data$term)
loan_data$term = gsub('60 months', "장기", loan_data$term)

pur_out = CrossTable(loan_data$purpose, loan_data$outcome, prop.c = F, prop.chisq = F, prop.t = F)
pur_out
pur_term= CrossTable(loan_data$purpose, loan_data$term, prop.c = F, prop.chisq = F, prop.t = F)
pur_term
home_out = CrossTable(loan_data$home_ownership, loan_data$outcome, prop.c = F, prop.chisq = F, prop.t = F)
home_out

# install.packages('lmPerm')
library(lmPerm)
summary(aov(grade ~ purpose, data = loan_data))
t.test(grade ~ outcome,data = loan_data, alternative = 'greater')

# 분석
loan = read.csv(file.path(PSDS_PATh, 'data', 'loan_data.csv'))

sample_n = sample(1:nrow(loan), size=round(0.3*nrow(loan)))
train = loan[-sample_n, ]; dim(train) # 31730
test = loan[sample_n, ]; dim(test) # 13603

# 나이브 베이즈
library(klaR)
library(caret)

naive_model = NaiveBayes(outcome ~ loan_amnt + term + annual_inc +
                         grade + purpose + home_ownership
                         , data=na.omit(train))
naive_model$table
result = predict(naive_model, test)
head(result$class, 10)

confusionMatrix(test$outcome, result$class)

# 로지스틱 회귀분석
logis_model = glm(outcome ~ loan_amnt + term + annual_inc +
                  grade + purpose + home_ownership, 
                  data=na.omit(train), family='binomial')
result = predict(logis_model)
summary(result)

# KNN

# XGboost
