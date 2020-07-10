
##########################################################
#  참고자료 
# 

# ROC곡선 
require('ROCR')

# ROCR packages 함수 확인 
ls("package:ROCR")  
#[1] "performance" "plot"        "prediction" 

cls = c('P', 'P', 'N', 'P', 'P', 'P', 'N', 'N', 'P', 'N', 'P', 
        'N', 'P', 'N', 'N', 'N', 'P', 'N', 'P', 'N')
score = c(0.9, 0.8, 0.7, 0.6, 0.55, 0.51, 0.49, 0.43, 
          0.42, 0.39, 0.33, 0.31, 0.23, 0.22, 0.19, 
          0.15, 0.12, 0.11, 0.04, 0.01)

pred = prediction(score, cls)
roc = performance(pred, measure="tpr", x.measure="fpr") # tpr: True positive rate
# fpr: False positive rate
roc.x = unlist(slot(roc, 'x.values'))
roc.y = unlist(slot(roc, 'y.values'))

# ROC : tpr & fpr 
plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="True Positive Rate", 
     xlab="False Positive Rate")
lines(x=roc.x, y=roc.y, col="orange", lwd=2)


# AUC (The Area Under an ROC Curve)
auc <- performance(pred, measure="auc")
auc <- auc@y.values[[1]]
auc
#[1] 0.68



# 이득도표 그래프 그릭, ROC : tpr & rpp  
gain = performance(pred, measure="tpr", x.measure="rpp")  # tpr: True positive rate
# rpp: Rate of positive predictions
gain.x = unlist(slot(gain, 'x.values'))
gain.y = unlist(slot(gain, 'y.values'))
plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="True Positive Rate", xlab="Rate of Positive Predictions")
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)
lines(x=gain.x, y=gain.y, col="orange", lwd=2)



# 향상도 곡선 그리기 
lift = performance(pred, measure="lift", x.measure="rpp")  # lift: Lift value
# rpp: Rate of positive predictions
lift.x = unlist(slot(lift, 'x.values'))
lift.y = unlist(slot(lift, 'y.values'))
plot(lift, col="orange", lwd=2, xlim=c(0.1, 1.0), ylim=c(1.0, 2.2))
abline(h=1, col="red")
abline(v=0.1, col="blue")



# 나이브 베이즈 
# packages load
library(e1071)
library(mlbench)

# HouseVote84 데이터 불러오기
data(HouseVotes84, package="mlbench")
str(HouseVotes84)
#'data.frame':	435 obs. of  17 variables:
#$ Class: Factor w/ 2 levels "democrat","republican": 2 2 1 1 1 1 1 2 2 1 ...
#$ V1   : Factor w/ 2 levels "n","y": 1 1 NA 1 2 1 1 1 1 2 ...
#$ V2   : Factor w/ 2 levels "n","y": 2 2 2 2 2 2 2 2 2 2 ...
#$ V3   : Factor w/ 2 levels "n","y": 1 1 2 2 2 2 1 1 1 2 ...
#$ V4   : Factor w/ 2 levels "n","y": 2 2 NA 1 1 1 2 2 2 1 ...
#$ V5   : Factor w/ 2 levels "n","y": 2 2 2 NA 2 2 2 2 2 1 ...
#$ V6   : Factor w/ 2 levels "n","y": 2 2 2 2 2 2 2 2 2 1 ...
#$ V7   : Factor w/ 2 levels "n","y": 1 1 1 1 1 1 1 1 1 2 ...
#$ V8   : Factor w/ 2 levels "n","y": 1 1 1 1 1 1 1 1 1 2 ...
#$ V9   : Factor w/ 2 levels "n","y": 1 1 1 1 1 1 1 1 1 2 ...
#$ V10  : Factor w/ 2 levels "n","y": 2 1 1 1 1 1 1 1 1 1 ...
#$ V11  : Factor w/ 2 levels "n","y": NA 1 2 2 2 1 1 1 1 1 ...
#$ V12  : Factor w/ 2 levels "n","y": 2 2 1 1 NA 1 1 1 2 1 ...
#$ V13  : Factor w/ 2 levels "n","y": 2 2 2 2 2 2 NA 2 2 1 ...
#$ V14  : Factor w/ 2 levels "n","y": 2 2 2 1 2 2 2 2 2 1 ...
#$ V15  : Factor w/ 2 levels "n","y": 1 1 1 1 2 2 2 NA 1 NA ...
#$ V16  : Factor w/ 2 levels "n","y": 2 NA 1 2 2 2 2 2 2 NA ...

# 반응변수 분할표 
table(HouseVotes84$Class)
#democrat republican 
#     267        168
prop.table(table(HouseVotes84$Class))
#  democrat republican 
# 0.6137931  0.3862069 


# HouseVotes84 데이터 요약 
summary(HouseVotes84)
#       Class        V1         V2         V3         V4         V5         V6     
#democrat  :267   n   :236   n   :192   n   :171   n   :247   n   :208   n   :152  
#republican:168   y   :187   y   :195   y   :253   y   :177   y   :212   y   :272  
#                 NA's: 12   NA's: 48   NA's: 11   NA's: 11   NA's: 15   NA's: 11  
#   V7         V8         V9        V10        V11        V12        V13     
#n   :182   n   :178   n   :206   n   :212   n   :264   n   :233   n   :201  
#y   :239   y   :242   y   :207   y   :216   y   :150   y   :171   y   :209  
#NA's: 14   NA's: 15   NA's: 22   NA's:  7   NA's: 21   NA's: 31   NA's: 25  
#   V14        V15        V16     
# n   :170   n   :233   n   : 62  
# y   :248   y   :174   y   :269  
# NA's: 17   NA's: 28   NA's:104

# 속성별 결측치 집계 
colSums(is.na(HouseVotes84)) 
#Class    V1    V2    V3    V4    V5    V6    V7    V8    V9   V10   V11   V12 
#    0    12    48    11    11    15    11    14    15    22     7    21    31 
#V13   V14   V15   V16 
# 25    17    28   104

# HouseVotes84 자료를 훈련용 데이터(80%) 분리하기 
library(caret)
parts <- createDataPartition(HouseVotes84$Class, p=0.8)    
data.train <- HouseVotes84[parts$Resample1, ]
table(data.train$Class)
#democrat republican 
#     214        135 
prop.table(table(data.train$Class))
#  democrat republican 
# 0.6131805  0.3868195 

# HouseVotes84 자료를 테스트 데이터(20%)로 분리하기
data.test <- HouseVotes84[-parts$Resample1, ]
table(data.test$Class)
#democrat republican 
#      53         33
prop.table(table(data.test$Class))
#  democrat republican 
# 0.6162791  0.3837209 


# 훈련용 데이터로 나이브 베이즈 모델을 생성하기
nai.fit <- naiveBayes(Class~., data=data.train)

# 테스트 데이터로 예측을 수행하고, 나이브 베이즈 모델의 성능 평가하기
nai.pred <- predict(nai.fit, data.test[,-1], type='class')
nai.tb <- table(nai.pred, data.test$Class)
nai.tb
#nai.pred     democrat republican
#   democrat        50          2
#  republican        3         31

# 정분류율(accuracy)은 약 0.942 
mean(data.test$Class == nai.pred)      # accuracy
#[1] 0.9418605

# 오분류율(error rate)은 약 0.058 
(1-sum(diag(nai.tb))/sum(nai.tb))      # error rate
#[1] 0.05813953


# ROC곡선, AUC 
library(ROCR)

# ROC : tpr & fpr 
nb.pred <- prediction(as.integer(nai.pred), as.integer(data.test$Class))
roc     <- performance(nb.pred, measure = "tpr", x.measure = "fpr")
roc.x = unlist(slot(roc, 'x.values'))
roc.y = unlist(slot(roc, 'y.values'))
plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="True Positive Rate", xlab="False Positive Rate")
lines(x=roc.x, y=roc.y, col="orange", lwd=2)

# AUC (The Area Under an ROC Curve)
auc <- performance(nb.pred, measure="auc")
auc <- auc@y.values[[1]]
auc
#[1] 0.9413951



# 로지스틱 회귀 모델 
#
# 유방암(BreastCancer) 데이터 세트 불러오기 
library(mlbench)
data("BreastCancer")
str(BreastCancer)
#'data.frame':	699 obs. of  11 variables:
#$ Id              : chr  "1000025" "1002945" "1015425" "1016277" ...
#$ Cl.thickness     : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 5 5 3 6 4 8 1 2 2 4 ...
#$ Cell.size         : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 1 1 2 ...
#$ Cell.shape       : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 2 1 1 ...
#$ Marg.adhesion  : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 5 1 1 3 8 1 1 1 1 ...
#$ Epith.c.size      : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 2 7 2 3 2 7 2 2 2 2 ...
#$ Bare.nuclei      : Factor w/ 10 levels "1","2","3","4",..: 1 10 2 4 1 10 10 1 1 1 ...
#$ Bl.cromatin     : Factor w/ 10 levels "1","2","3","4",..: 3 3 3 3 3 9 3 3 1 2 ...
#$ Normal.nucleoli : Factor w/ 10 levels "1","2","3","4",..: 1 2 1 7 1 7 1 1 1 1 ...
#$ Mitoses        : Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 5 1 ...
#$ Class           : Factor w/ 2 levels "benign","malignant": 1 1 1 1 1 2 1 1 1 1 ...
table(BreastCancer$Class)
# benign malignant 
#    458       241
prop.table(table(BreastCancer$Class))
#    benign malignant 
# 0.6552217 0.3447783 


# 결측값(NA) 확인 및 제거하기
colSums(is.na(BreastCancer)) 
# Id    Cl.thickness       Cell.size      Cell.shape   Marg.adhesion    Epith.c.size 
#  0               0               0               0               0               0 
# Bare.nuclei     Bl.cromatin Normal.nucleoli         Mitoses           Class 
#          16               0               0               0               0 
sum(is.na(BreastCancer)) 
#[1] 16
BreastCancer2 <- BreastCancer[complete.cases(BreastCancer),]
sum(is.na(BreastCancer2))
#[1] 0


# 중복 데이터(Duplicated data) 확인 및 제거하기
nrow(BreastCancer2)
#[1] 683
sum(duplicated(BreastCancer2)) 
#[1] 8
BreastCancer3 <- BreastCancer2[!duplicated(BreastCancer2), ]
nrow(BreastCancer3)
#[1] 675
sum(duplicated(BreastCancer3))  
#[1] 0


# 반응변수 구성 분포 확인
table(BreastCancer3$Class); cat("total :", margin.table(table(BreastCancer3$Class)))
# benign malignant 
#    439       236 
# total : 675
prop.table(table(BreastCancer3$Class)) 
#    benign malignant 
# 0.6503704 0.3496296


# 반응변수(Class)를 Y, 설명변수를 X 라는 데이터프레임으로 분리하고, 
# - 악성(malignant)=1, 양성(benign)=0 
# 설명변수의 타입을 숫자 타입으로 변환하기 
Y <- ifelse(BreastCancer3$Class == 'malignant', 1, 0)
X <- BreastCancer3[,c(2:10)]
X$Cl.thickness    <- as.integer(X$Cl.thickness)
X$Cell.size        <- as.integer(X$Cell.size)
X$Cell.shape      <- as.integer(X$Cell.shape)
X$Marg.adhesion  <- as.integer(X$Marg.adhesion)
X$Epith.c.size     <- as.integer(X$Epith.c.size)
X$Bare.nuclei     <- as.integer(X$Bare.nuclei)
X$Bl.cromatin     <- as.integer(X$Bl.cromatin)
X$Normal.nucleoli <- as.integer(X$Normal.nucleoli)
X$Mitoses         <- as.integer(X$Mitoses)


# 설명변수 표준화하기 
X2 <- scale(X)


# 반응변수, 설명변수 qud합하기 
BreastCancer4 <- data.frame(Y, X2)
str(BreastCancer4)


# 데이터 세트(BreastCancer4)를 훈련용 데이터(80%)와 테스트 데이터(20%)로 분리하기 
set.seed(123) 
train <- sample(1:nrow(BreastCancer4), size=0.8*nrow(BreastCancer4), replace=F)
test <- (-train)
Y.test <- Y[test]
scales::percent(length(train)/nrow(BreastCancer4))
#[1] "80%"
head(train)
#[1] 195 532 276 594 632  31


# 훈련용 데이터로 로지스틱 회귀 모델 적합하기
glm.fit <- glm(Y~., data=BreastCancer4, family=binomial(link="logit"), subset=train)
summary(glm.fit)
#
#Call:
#  glm(formula = Y ~ ., family = binomial(link = "logit"), data = BreastCancer4, 
#      subset = train)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.3535  -0.1344  -0.0686   0.0216   2.3780  
#
#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -1.00864    0.36832  -2.738 0.006172 ** 
#Cl.thickness     1.41131    0.43279   3.261 0.001110 ** 
#Cell.size       -0.05671    0.68019  -0.083 0.933554    
#Cell.shape       0.87409    0.72839   1.200 0.230128    
#Marg.adhesion    0.98337    0.37274   2.638 0.008334 ** 
#Epith.c.size     0.43534    0.36779   1.184 0.236547    
#Bare.nuclei      1.47806    0.39113   3.779 0.000158 ***
#Bl.cromatin      0.80216    0.44001   1.823 0.068294 .  
#Normal.nucleoli  0.58023    0.35380   1.640 0.101007    
#Mitoses          0.84191    0.69655   1.209 0.226778    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 691.519  on 539  degrees of freedom
#Residual deviance:  92.505  on 530  degrees of freedom
#AIC: 112.5
#
#Number of Fisher Scoring iterations: 8


# 설명변수 제거를 위해 후진 소거법(backward elimination)을 사용해서 모델을 적합하기 
step(glm.fit, direction="backward")
#Start:  AIC=112.5
#Y ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size + 
#  Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses
#
#Df Deviance    AIC
#- Cell.size        1   92.512 110.51
#- Cell.shape       1   93.859 111.86
#- Epith.c.size     1   93.895 111.89
#<none>                 92.505 112.50
#- Normal.nucleoli  1   95.290 113.29
#- Mitoses          1   95.318 113.32
#- Bl.cromatin      1   95.993 113.99
#- Marg.adhesion    1   99.899 117.90
#- Cl.thickness     1  105.193 123.19
#- Bare.nuclei      1  109.832 127.83
#
#Step:  AIC=110.51
#Y ~ Cl.thickness + Cell.shape + Marg.adhesion + Epith.c.size + 
#  Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses
#
#Df Deviance    AIC
#- Epith.c.size     1   93.899 109.90
#<none>                 92.512 110.51
#- Cell.shape       1   95.176 111.18
#- Normal.nucleoli  1   95.297 111.30
#- Mitoses          1   95.339 111.34
#- Bl.cromatin      1   96.085 112.08
#- Marg.adhesion    1  100.038 116.04
#- Cl.thickness     1  105.326 121.33
#- Bare.nuclei      1  109.842 125.84
#
#Step:  AIC=109.9
#Y ~ Cl.thickness + Cell.shape + Marg.adhesion + Bare.nuclei + 
#  Bl.cromatin + Normal.nucleoli + Mitoses
#
#Df Deviance    AIC
#<none>                 93.899 109.90
#- Mitoses          1   96.757 110.76
#- Normal.nucleoli  1   97.143 111.14
#- Cell.shape       1   97.833 111.83
#- Bl.cromatin      1   98.440 112.44
#- Marg.adhesion    1  102.992 116.99
#- Cl.thickness     1  107.009 121.01
#- Bare.nuclei      1  112.294 126.29
#
#Call:  glm(formula = Y ~ Cl.thickness + Cell.shape + Marg.adhesion + 
#  Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, family = binomial(link = "logit"), 
#  data = BreastCancer4, subset = train)
#
#Coefficients:
#  (Intercept)     Cl.thickness       Cell.shape    Marg.adhesion      Bare.nuclei      Bl.cromatin  
#      -0.9608           1.4003           0.9914           1.0430           1.5215           0.8882  
#Normal.nucleoli          Mitoses  
#         0.6295           0.7982  
#
#Degrees of Freedom: 539 Total (i.e. Null);  532 Residual
#Null Deviance:	    691.5 
#Residual Deviance: 93.9 	AIC: 109.9



# 후진소거법의 3모델(Cell.size, Epith.c.size 설명변수 제거)을 적합하고, 모델의 유의성 검정하기
glm.fit2 <- glm(Y~ Cell.shape+Normal.nucleoli+Mitoses+Bl.cromatin+Marg.adhesion+
                  Cl.thickness+Bare.nuclei, 
                data=BreastCancer4, family=binomial(link="logit"), subset=train)
anova(glm.fit2, test="Chisq")
#Analysis of Deviance Table
#
#Model: binomial, link: logit
#
#Response: Y
#
#Terms added sequentially (first to last)
#
#
#                Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#NULL                              539     691.52              
#Cell.shape       1   471.35       538     220.17 < 2.2e-16 ***
#Normal.nucleoli  1    31.21       537     188.96 2.315e-08 ***
#Mitoses          1    17.66       536     171.30 2.648e-05 ***
#Bl.cromatin      1    24.47       535     146.84 7.567e-07 ***
#Marg.adhesion    1    14.34       534     132.50 0.0001526 ***
#Cl.thickness     1    20.20       533     112.29 6.970e-06 ***
#Bare.nuclei      1    18.40       532      93.90 1.795e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# 테스트 데이터로 모델 성능 평가 수행하기
glm.probs <- predict(glm.fit.2, BreastCancer4[test,], type="response")
glm.pred <- ifelse(glm.probs > .5, 1, 0)
table(Y.test, glm.pred)
#      glm.pred
#Y.test  0  1
#     0 81  1
#     1  0 53
mean(Y.test == glm.pred)      # accuracy
#[1] 0.9925926
mean(Y.test != glm.pred)      # error rate
#[1] 0.007407407


# ROC곡선와 AUC 확인하기 
library(ROCR)

# ROC : tpr & fpr 
glm.pred <- prediction(glm.probs, Y.test)
roc <- performance(glm.pred, measure = "tpr", x.measure = "fpr")
roc.x = unlist(slot(roc, 'x.values'))
roc.y = unlist(slot(roc, 'y.values'))
plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="True Positive Rate", xlab="False Positive Rate")
lines(x=roc.x, y=roc.y, col="orange", lwd=2)

# AUC (The Area Under an ROC Curve)
auc <- performance(glm.pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
#[1] 0.9990796





