##########################################
# 참고자료   
#

choose(80, 4) * (0.5)^4 * (0.5)^(80-4)
#[1] 1.308252e-18
(x <- 0.000000000000000001308252)
#[1] 1.308252e-18



# 정규분포의 누적분포함수(cumulative function of normal distribution) 값 계산
# Pr(Z > 3.16) = 1 - F(3.16)
1 - pnorm(q=c(3.16), mean=0, sd=1, lower.tail = TRUE)
# [1] 0.0007888457



# 정규분포의 누적분포함수(cumulative function of normal distribution) 값 계산
# Pr(Z < -2) = F(-2)
pnorm(q=c(-2), mean=0, sd=1)
#[1] 0.02275013



# p=0.05이고, 자유도가 4, 
# t분포 분위수 함수 : qt(p, df, lower.tail = TRUE/FALSE)   
qt(p=0.05, df=4, lower.tail = FALSE)
#[1] 2.131847

# 검정통계량
(540-1000)/(299/sqrt(5))
#[1] -3.440105
pt(q=c(-3.440105), df=4)
#[1] 0.0002907443



# 세 종류 데이터  
data1 <- c(30, -5, 55, -30, -20, 45)
data2 <- c(12, 13, 12, 13, 12, 13)
data3 <- c(30, -5, 55, -30, -20, 45, 30, -5, 55, -30, -20, 45)

# data1  평균, 표준편차 
mean(data1)
#[1] 12.5
sd(data1)
#[1] 35.60197

# data2  평균, 표준편차 
mean(data2)
#[1] 12.5
sd(data2)
#[1] 0.5477226

# data3  평균, 표준편차 
mean(data3)
#[1] 12.5
sd(data3)
#[1] 33.94514

# t-검정
t.test(data1, alternative = c("greater"))
#
#One Sample t-test
#
#data:  data1
#t = 0.86003, df = 5, p-value = 0.2145
#alternative hypothesis: true mean is greater than 0
#95 percent confidence interval:
#  -16.7876      Inf
#sample estimates:
#  mean of x 
#  12.5 

# t-value
(12.5)/(35.60197/sqrt(6))
#[1] 0.8600261
# p-value
1- pt(q=c(0.8600261), df=5)
#[1] 0.214537


# t-검정
t.test(data2, alternative = c("greater"))
#
#One Sample t-test
#
#data:  data2
#t = 55.902, df = 5, p-value = 1.732e-08
#alternative hypothesis: true mean is greater than 0
#95 percent confidence interval:
#  12.04942      Inf
#sample estimates:
#  mean of x 
#       12.5 

# t-value 
(12.5)/(0.5477226/sqrt(6))
#[1] 55.9017
# p-value
1- pt(q=c(55.9017), df=5)
#[1] 1.732451e-08


# t-검정
t.test(data3, alternative = c("greater"))
#
#One Sample t-test
#
#data:  data3
#t = 1.2756, df = 11, p-value = 0.1142
#alternative hypothesis: true mean is greater than 0
#95 percent confidence interval:
#  -5.098089       Inf
#sample estimates:
#  mean of x 
#       12.5 

# t-value
(12.5)/(33.94514/sqrt(12))
#[1] 1.275625
# p-value 
1- pt(q=c(1.275625), df=11)
#[1] 0.114184



# 카이제곱검정 - 적합도 검정 
obs <- c(20, 40, 40) 
obs.probs <- c(2/10, 3/10, 5/10)
(g.fit <- chisq.test(obs, p=obs.probs))
#
#     Chi-squared test for given probabilities
#
#data:  obs
#X-squared = 5.3333, df = 2, p-value = 0.06948

g.fit$observed    # observed frequency 
#[1] 20 40 40
g.fit$expected    # expected frequeycy 
#[1] 20 30 50
g.fit$statistic   # chi-squared statistics
#X-squared 
# 5.333333 
g.fit$parameter   # degrees of freedom
#df 
# 2 
g.fit$p.value     # P-value
#[1] 0.06948345

# 카이제곱분포의 누적 분포 함수를 이용한 p-value 계산 
1- pchisq(q=c(5.3333), df=2, lower.tail=TRUE)
#[1] 0.06948461



# 카이제곱검정 - 독립성 검정
raw_data <- c(7, 13, 9, 12, 13, 21, 10, 19, 11, 18, 12, 13)
data_mtx <- matrix(raw_data, byrow=TRUE, nrow=3)
data_mtx
#     [,1] [,2] [,3] [,4]
#[1,]    7   13    9   12
#[2,]   13   21   10   19
#[3,]   11   18   12   13

dimnames(data_mtx) <- list("Class" = c("Class1", "Class2", "Class3"), 
                           "Score" = c("ScoreA", "ScoreB", "ScoreC", "ScoreF"))
data_mtx
#        Score
#Class    ScoreA ScoreB ScoreC ScoreF
#  Class1      7     13      9     12
#  Class2     13     21     10     19
#  Class3     11     18     12     13

# marginal distribution : addmargins() 
addmargins(data_mtx)                       
#        Score
#Class    ScoreA ScoreB ScoreC ScoreF Sum
#  Class1      7     13      9     12  41
#  Class2     13     21     10     19  63
#  Class3     11     18     12     13  54
#  Sum        31     52     31     44 158

# proportional distribution : prop.table()
addmargins(prop.table(data_mtx)) 
#        Score
#Class        ScoreA     ScoreB     ScoreC     ScoreF       Sum
#  Class1 0.04430380 0.08227848 0.05696203 0.07594937 0.2594937
#  Class2 0.08227848 0.13291139 0.06329114 0.12025316 0.3987342
#  Class3 0.06962025 0.11392405 0.07594937 0.08227848 0.3417722
#  Sum    0.19620253 0.32911392 0.19620253 0.27848101 1.0000000

# bar plot 
barplot(t(data_mtx), beside=TRUE, legend=TRUE, 
        ylim=c(0, 30), 
        ylab="Observed frequencies in sample", 
        main="Frequency of math score by class")

# chisquared test : chisq.test()
(i.fit <- chisq.test(data_mtx))
#
#       Pearson's Chi-squared test
#
#data:  data_mtx
#X-squared = 1.3859, df = 6, p-value = 0.9667

# 카이제곱분포의 누적 분포 함수를 이용한 p-value 계산 
1- pchisq(q=c(1.3859), df=6, lower.tail=TRUE)
#[1] 0.9667105



# 카이제곱검정 - 동질성 검정
raw_data <- c(50, 30, 20, 50, 80, 70)
data_mtx <- matrix(raw_data, byrow=TRUE, nrow=2)
data_mtx
#     [,1] [,2] [,3]
#[1,]   50   30   20
#[2,]   50   80   70

dimnames(data_mtx) <- list("성별" = c("남학생", "여학생"), 
                           "DS교과목" = c("통계", "머신러닝", "딥러닝"))
data_mtx
#        DS교과목
#성별     통계 머신러닝 딥러닝
#  남학생   50       30     20
#  여학생   50       80     70

# marginal distribution : addmargins() 
addmargins(data_mtx)
#        DS교과목
#성별     통계 머신러닝 딥러닝 Sum
#  남학생   50       30     20 100
#  여학생   50       80     70 200
#  Sum     100      110     90 300

# proportional distribution : prop.table() 
addmargins(prop.table(data_mtx))
#        DS교과목
#성별          통계  머신러닝     딥러닝       Sum
#  남학생 0.1666667 0.1000000 0.06666667 0.3333333
#  여학생 0.1666667 0.2666667 0.23333333 0.6666667
#  Sum    0.3333333 0.3666667 0.30000000 1.0000000

# bar plot : barplot()
barplot(t(data_mtx), beside=TRUE, legend=TRUE, 
        ylim=c(0, 120), 
        ylab="Observed frequencies in sample", 
        main="데이터 사이언스 교과목 선호 조사 결과")

# chisquared test : chisq.test() 
(h.fit <- chisq.test(data_mtx))
#
#     Pearson's Chi-squared test
#
#data:  data_mtx
#X-squared = 19.318, df = 2, p-value = 6.384e-05

# 카이제곱분포의 누적 분포 함수를 이용한 p-value 계산 
1- pchisq(q=c(19.318), df=2, lower.tail=TRUE)
#[1] 6.384834e-05

