# 200709

getwd()
setwd("C:/Users/Admin/Desktop/공공 빅데이터 청년 인턴십/R")

#####################################################################################
# 회귀와 예측
#####################################################################################

# 단순선형회귀
PSDS_PATh = file.path(('C:/Users/Admin/Desktop/공공 빅데이터 청년 인턴십/R'))
lung = read.csv(file.path(PSDS_PATh, 'data', 'LungDisease.csv'))

dim(lung); str(lung); head(lung)

plot(lung$Exposure, lung$PEFR, xlab="Exposure", ylab="PEER")

(model = lm(PEFR ~ Exposure, data=lung))
summary(model)

fitted = predict(model); head(fitted) # 에측값
resid = residuals(model); head(resid) # 잔차

# 다중선형회귀
house = read.csv(file.path(PSDS_PATh, 'data', 'house_sales.csv'), sep='\t')

dim(house); str(house)
head(house[, c('AdjSalePrice', 'SqFtTotLiving', 'SqFtLot', 
               'Bathrooms', 'Bedrooms', 'BldgGrade')])

(house_lm = lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade, 
              data=house, na.action=na.omit))
summary(house_lm)

(house_full = lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                  Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + 
                  SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction, 
                data=house, na.action=na.omit))
summary(house_full)

# 단게적 회귀
library(MASS)
step_lm = stepAIC(house_full, direction='both')

# 교란변수 : ZipGroup

# 상호작용항 : SqFtTotLiving * ZipGroup 추가

#####################################################################################
# 분류
#####################################################################################

# 나이브베이즈 : X, Y 모두 범주형일 때
loan_data = read.csv(file.path(PSDS_PATh, 'data', 'loan_data.csv'))
dim(loan_data); str(loan_data); head(loan_data)

library(klaR)
naive_model = NaiveBayes(outcome ~ purpose_ + home_ + emp_len_, data=na.omit(loan_data))
naive_model$tables

(new_loan = loan_data[147, c('purpose_', 'home_', 'emp_len_')])
row.names(new_loan) = NULL
predict(naive_model, new_loan) # 예측값

# 로지스틱 회귀
logis_model = glm(outcome ~ payment_inc_ratio + purpose_ + home_ + emp_len_ + borrower_score, 
                  data=na.omit(loan_data), family='binomial')
summary(logis_model)
