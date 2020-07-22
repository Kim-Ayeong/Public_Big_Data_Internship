##### 실습2 - 청소년범죄 영향인자 분석

######## 비교시각화 통한 주요인자 도출 #######
##############################################

# 데이터셋 업로드
teenager_crime <- read.csv("../data/crime_teenager.csv")
View(teenager_crime)
str(teenager_crime)
rownames(teenager_crime) <- teenager_crime[[1]]
 
#스타차트
stars(teenager_crime[ ,2:6]  )
 
#나이팅게일차트
stars(teenager_crime[ ,2:6], flip.labels = FALSE, draw.segments = TRUE)

#평행좌표계
parallelplot(teenager_crime[ ,2:6], horizontal.axis=FALSE)

#평행좌표계(참고자료)
education <- read.csv("http://datasets.flowingdata.com/education.csv", header=TRUE) ##미국 SAT 점수 분석
View(education)
parallelplot(education[,2:7], horizontal.axis=FALSE)

#히트맵
teenager_crime_matrix<-data.matrix(teenager_crime[,2:6])
heatmap(teenager_crime_matrix,Rowv=NA, col=brewer.pal(9,"Blues"), scale="column", margin=c(5,10))



######## 관계시각화를 통한 인자 간 상관성 분석 #######
######################################################

#1. 청소년 강도범죄와 살인범죄간의 상관성 확인
plot(teenager_crime$robbery, teenager_crime$murder)

#2. 청소년 절도범죄와 폭력범죄간의 상관성 확인
plot(teenager_crime$thief, teenager_crime$violence)

# 범죄간의 상관성 분석-(참고: 교재 코드의 scatterplot_matrix 대신 pair 사용함)
#scatterplot.matrix(teenager_crime[,2:6]) #
pairs(teenager_crime[,2:6] ) 
 

# 상관계수 계산
teenager_crime_cor <-teenager_crime[ ,c(2:6)]
sum(is.na(teenager_crime_cor))
teenager_crime_cor_value <- cor(teenager_crime_cor)
teenager_crime_cor_value

#corrplot이용(1)
corrplot(teenager_crime_cor_value, method="circle")
#corrplot이용(2)
corrplot(teenager_crime_cor_value, method="circle", addCoef.col = "red") #상관계수 숫자 색

#psych패키지 이용
install.packages("psych")
library(psych)
pairs.panels(teenager_crime[ ,2:6])

# 유흥업소수와 범죄간의 상관성 분석 위한 관계시각화 
nightlife_crime <- read.csv("C:/BigPublic/data/nightlife_crime.csv")
head(nightlife_crime)
str(nightlife_crime)
colnames(nightlife_crime)

nightlife_crime_cor = nightlife_crime[, -1]
colnames(nightlife_crime_cor)

#상관계수 구하기
cortest = corr.test(nightlife_crime_cor)[1]
cortest
 
#상관계수 시각화
pairs.panels(nightlife_crime_cor[,2:13])  
