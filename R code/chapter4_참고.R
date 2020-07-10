##########################################################
#  참고자료 
#

PSDS_PATH <- file.path('.', 'source')
# 대학생 92명의 키와 몸무게 데이터 읽기 
std90 <- read.table(file.path(PSDS_PATH, "data", "student90.csv"), 
                    sep = ",", 
                    stringsAsFactors = FALSE, 
                    header = TRUE, 
                    na.strings = "")
nrow(std90)
#[1] 90
head(std90)
#  no sex weight_kg height_cm
#1  1   m           98          198
#2  2   m           77          170
#3  3   m           70          170
#4  4   m           90          198
#5  5   m           71          170
#6  6   m           70          165



# 단순 선형 회귀 모델 생성 
(m <- lm(weight_kg ~ height_cm, data = std90))
#
#Call:
#  lm(formula = weight_kg ~ height_cm, data = std90)
#
#Coefficients:
#  (Intercept)    height_cm  
#     32.6604         0.2247 


# 회귀 계수 구하기 
coef(m)
#  (Intercept)   height_cm 
# 32.6604144   0.2246605 


# 대학생 90명 데이터의 1~4번째 적합(예측)된 값 확인하기 : fitted(model)
fitted(m)[1:4]
#            1             2              3             4 
# 77.14319   70.85270   70.85270   77.14319 

# 학생의 몸무게(kg) = 32.66 + 0.224 * 학생의 키(cm) 
((32.6604144) + (0.2246605) * (std90$height_cm[1:4]))
#            1             2              3             4 
# 77.14319   70.85270   70.85270   77.14319 


# 키와 몸림무게의 이상값 그리 
plot(m, which = 4)


# 이상값 진단 
x_cooks.d <- cooks.distance(m)
x_cooks.d[1:4]
#           1            2            3            4 
#5.992961e-02 1.202838e-03 2.314356e-05 2.277257e-02 

NROW(x_cooks.d)
#[1] 90

x_cooks.d[which(x_cooks.d>qf(0.5, df1 = 2, df2 = 88))]
#named numeric(0)


# 이상값 진단 
library(car)
outlierTest(m)
#No Studentized residuals with Bonferonni p < 0.05
#Largest |rstudent|:
#    rstudent unadjusted p-value  Bonferonni p
# 90 2.709609          0.0081125       0.73013



# 대학생 90명 데이터의 1~4번째 잔차 구하기 : residuals(model) 
residuals(m)[1:4]
#               1              2              3               4 
# 20.8568064  6.1473004 -0.8526996 12.8568064 

# 실제 데이터 값 = 적합된 값 + 잔차 
# 대학생 90명 데이터의 1 ~ 4번째 실제 몸무게 
std90$weight_kg[1:4]
#  98 77 70 90

# 대학생 90명 데이터의 1 ~ 4번째 적합된 값 + 잔차 
fitted(m)[1:4] + residuals(m)[1:4]
#   1  2   3  4 
# 98 77 70 90 


# Q-Q plot
qqnorm(residuals(m))
qqline(residuals(m))


# 샤피로 윌크 검정: 일변수 자료에 대해 수치적으로 정규성을 검정하는 기법
shapiro.test(residuals(m))
#
#Shapiro-Wilk normality test
#
#data:  residuals(m)
#W = 0.98121, p-value = 0.2189


# 회귀 계수의 95% 신뢰구간 구하기 : confint(model)
confint(m, level = 0.95)
#                  2.5 %      97.5 %
# (Intercept) 4.68512548  60.6357032
# height_cm   0.05911794   0.3902031


# 신뢰구간 구하기 
m_conf <- predict(m, level = 0.95, interval = "confidence")
head(m_conf)
#       fit      lwr      upr
#1 77.14319 71.45341 82.83298
#2 70.85270 68.02003 73.68536
#3 70.85270 68.02003 73.68536
#4 77.14319 71.45341 82.83298
#5 70.85270 68.02003 73.68536
#6 69.72940 66.86626 72.59253


# 키와 몸무게 산포도, 추정된 평균 몸무게, 신뢰구간 
plot(weight_kg~height_cm, data=std90)
lwr <- m_conf[,2]   
upr <- m_conf[,3]   
sx <- sort(std90$height_cm, index.return=TRUE)
abline(coef(m), lwd=2)
lines(sx$x, lwr[sx$ix], col="blue", lty=2)
lines(sx$x, upr[sx$ix], col="blue", lty=2)


# 키와 몸무게의 예측구간 
m_pred <- predict(m, level = 0.95, interval = "predict")
head(m_pred)
#       fit      lwr       upr
#1 77.14319 49.83131 104.45507
#2 70.85270 43.99029  97.71511
#3 70.85270 43.99029  97.71511
#4 77.14319 49.83131 104.45507
#5 70.85270 43.99029  97.71511
#6 69.72940 42.86376  96.59504



# 키와 몸무게 산포도, 예측구간 
p_lwr <- m_pred[,2]   
p_upr <- m_pred[,3]   
lines(std90$height_cm, p_lwr, col="red", lty=2)
lines(std90$height_cm, p_upr, col="red", lty=2)


# 잔체 제곱 합 구하기 : deviance(model)
deviance(m)
#  15899.88


# 새로운 학생의 키가 175cm 라면, 예상되는 몸무게 구하기
predict(m, newdata = data.frame(height_cm=175), interval = "confidence")
#    fit        lwr        upr
# 71.976   68.93945   75.01255


# 모델 평가 
summary(m)
#
#Call:
#  lm(formula = weight_kg ~ height_cm, data = std90)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-30.020  -8.460  -1.066   6.918  34.654 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  32.6604    14.0771   2.320  0.02265 * 
#height_cm     0.2247     0.0833   2.697  0.00838 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 13.44 on 88 degrees of freedom
#Multiple R-squared:  0.07635,	Adjusted R-squared:  0.06585 
#F-statistic: 7.274 on 1 and 88 DF,  p-value: 0.008385



#
library(modelr)

rmse(m, std90)    # root-mean-squared-error
#[1] 13.29155
rsquare(m, std90) # the variance of the predictions divided by the variance of the response.
#[1] 0.0763455
mae(m, std90)     # mean absolute error 
#[1] 10.45572
qae(m, std90)     # quantiles of absolute error.
#       5%        25%        50%        75%        95% 
#0.4215742  3.7722217  7.8746441 15.2293971 24.6834396 


#
(m_a <- lm(weight_kg ~ height_cm, data = std90))
#Call:
#  lm(formula = weight_kg ~ height_cm, data = std90)
#Coefficients:
#  (Intercept)    height_cm  
#      32.6604         0.2247  

(m_b <- lm(weight_kg ~ 1, data = std90))
#Call:
#  lm(formula = weight_kg ~ 1, data = std90)
#Coefficients:
#  (Intercept)  
#         70.43 

rmse(m_a, std90)    # root-mean-squared-error
#[1] 13.29155
rmse(m_b, std90)    # root-mean-squared-error
#[1] 13.82996

mae(m_a, std90)     # mean absolute error 
#[1] 10.45572
mae(m_b, std90)     # mean absolute error 
#[1] 10.66296


# 두 모델 비교 결과 
anova(m_a, m_b)
#Analysis of Variance Table
#
#Model 1: weight_kg ~ height_cm
#Model 2: weight_kg ~ 1
#  Res.Df   RSS Df Sum of Sq      F   Pr(>F)   
#1     88 15900                                
#2     89 17214 -1   -1314.2 7.2737 0.008385 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# 다중 선형 회귀 : trees data
str(trees)
#'data.frame':	31 obs. of  3 variables:
#$ Girth : num  8.3 8.6 8.8 10.5 10.7 10.8 11 11 11.1 11.2 ...
#$ Height: num  70 65 63 72 81 83 66 75 80 75 ...
#$ Volume: num  10.3 10.3 10.2 16.4 18.8 19.7 15.6 18.2 22.6 19.9 ...

summary(trees)
#    Girth           Height       Volume     
#Min.   : 8.30   Min.   :63   Min.   :10.20  
#1st Qu.:11.05   1st Qu.:72   1st Qu.:19.40  
#Median :12.90   Median :76   Median :24.20  
#Mean   :13.25   Mean   :76   Mean   :30.17  
#3rd Qu.:15.25   3rd Qu.:80   3rd Qu.:37.30  
#Max.   :20.60   Max.   :87   Max.   :77.00  


# trees 데이터의 분포  
library(scatterplot3d)
scatterplot3d(trees$Girth, trees$Height, trees$Volume)


# 다중 선형 회귀 모델 생설하기 
m <- lm(Volume ~ Girth + Height, data = trees)
m
#
#Call:
#  lm(formula = Volume ~ Girth + Height, data = trees)
#
#Coefficients:
#  (Intercept)        Girth       Height  
#     -57.9877       4.7082       0.3393  


# trees 데이터와 회귀 모델을 중첩하여 시각화 
s <- scatterplot3d(trees$Girth, trees$Height, trees$Volume, pch = 20, type = 'h', 
                   angle = 55)
s$plane3d(m)


# 벗나무 세 그루의 지름과 키를 측정하여 부피를 예상하기  
(n.data <- data.frame(Girth=c(8.5, 13.0, 19.0), Height=c(72, 86, 85)))
(n.y <- predict(m, newdata = n.data))
#       1         2         3 
#6.457794 32.394034 60.303746 


-57.9877 + 4.7082*8.5 + 0.3393*72
#[1] 6.4616
-57.9877 + 4.7082*13.0 + 0.3393*86
#[1] 32.3987
-57.9877 + 4.7082*19.0 + 0.3393*85
#[1] 60.3086


# 벗나무 세 그루의 지름과 키를 측정하여 부피를 예상한 결과 시각화   
s <- scatterplot3d(c(8.5, 13.0, 19.0), c(72, 86, 85), n.y, pch = 20, type = 'h', 
                   color = 'red', angle = 55)
s$plane3d(m)

