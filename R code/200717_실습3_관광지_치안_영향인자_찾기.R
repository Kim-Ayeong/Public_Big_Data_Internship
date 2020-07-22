##### 실습3 관광지 치안 영향인자 찾기 

######## 비교시각화를 통한 주요인자 도출 #######
################################################
library(data.table)
# 데이터셋 업로드
df_1.crim <- fread("C:/BigPublic/data/gimhae/01_gimhae_crime.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)
df_2.kara <- fread("C:/BigPublic/data/gimhae/02_gimhae_garaoke.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)
df_3.cctv <- fread("C:/BigPublic/data/gimhae/03_gimhae_CCTV.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)
df_4.foreign <- fread("C:/BigPublic/data/gimhae/04_gimhae_foreigner.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)
df_5.police <-fread("C:/BigPublic/data/gimhae/05_gimhae_policeman.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)
df_6.visitor <- fread("C:/BigPublic/data/gimhae/06_gimhae_tourlist.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)


### 연도별 김해시 범죄 발생 현황분석

# 데이터셋 업로드
df_1.crim <- fread("C:/BigPublic/data/gimhae/01_gimhae_crime.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)

# 데이터 보기
View(df_1.crim)
str(df_1.crim)

row.names(df_1.crim) <- df_1.crim[,1]

# star plot
stars(df_1.crim[, 2:ncol(df_1.crim)], key.loc = c(7, 2.5))

# 나이팅게일
stars(df_1.crim[, 2:ncol(df_1.crim)], flip.labels = T, draw.segments = T, key.loc = c(7, 2.5))

# heatmap
df_1.crim_matrix <- data.matrix(df_1.crim[, 2:ncol(df_1.crim)])
df_1.crim_matrix <- na.omit(df_1.crim_matrix)
heatmap(df_1.crim_matrix, Rowv=NA, col=brewer.pal(9,"Blues"), scale="column", margin=c(5,10))


### CCTV와 범죄 발생 유행 현황분석 

# 데이터셋 업로드
df_1.crim <- fread("C:/BigPublic/data/gimhae/01_gimhae_crime.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)
df_3.cctv <- fread("C:/BigPublic/data/gimhae/03_gimhae_CCTV.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)

# 데이터 보기
View(df_3.cctv)
str(df_3.cctv)

# 데이터 병합하기
df_crim_cctv <- merge(df_1.crim, df_3.cctv)

# 로우네임 지정하기
row.names(df_crim_cctv) <- df_crim_cctv[,1]  

# 데이터 보기
View(df_crim_cctv)

# 일부 데이터 선택하기
df_crim_cctv_sub <- df_crim_cctv[,-c(1:3)]

# 로우네임 지정하기
row.names(df_crim_cctv_sub) <- row.names(df_crim_cctv)

# 데이터 보기
View(df_crim_cctv_sub)

# star plot
stars(df_crim_cctv_sub[,2:ncol(df_crim_cctv_sub)], key.loc = c(7.5, 2.5))

# 나이팅게일
stars(df_crim_cctv_sub[, 2:ncol(df_crim_cctv_sub)], flip.labels = T, draw.segments = T, key.loc = c(8, 2.5))

# heatmap
df_cctv_visitor_matrix <- data.matrix(df_crim_cctv_sub[, 2:ncol(df_crim_cctv_sub)])
df_cctv_visitor_matrix <- na.omit(df_cctv_visitor_matrix)
df_cctv_visitor_matrix
heatmap(df_cctv_visitor_matrix,Rowv=NA, col=brewer.pal(9,"Greens"), scale="column", margin=c(5,10))
heatmap(df_cctv_visitor_matrix,Rowv=NA, Colv = NA,col=brewer.pal(9,"Greens"), scale="column", margin=c(5,10))


# 데이터 translate
df_crim_cctv_t <- data.frame(t(df_crim_cctv))
View(df_crim_cctv_t)

# 데이터 선택
df_crim_cctv_t <- df_crim_cctv_t[-1,]

# star plot
stars(df_crim_cctv_t[,2:ncol(df_crim_cctv_t)])

# 나이팅게일
stars(df_crim_cctv_t[, 2:ncol(df_crim_cctv_t)], flip.labels = T, draw.segments = T)


### 5대 범죄 현황 분석 

# 데이터셋 업로드
df_7.criminal <- fread("C:/BigPublic/data/gimhae/07_criminal.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)

# 데이터 보기
names(df_7.criminal)
rownames(df_7.criminal) <-df_7.criminal[[1]]
df_7.criminal <- df_7.criminal[-1]
df_7.criminal <- df_7.criminal[1:(nrow(df_7.criminal)-2), ]

df_7.criminal

# 스타차트
stars(df_7.criminal, key.loc = c(7,2.3))

# 나이팅게일
stars(df_7.criminal, flip.labels = T, draw.segments = T, key.loc = c(7,2.3))


### 범죄유형별 경향성 분석 

# 평행좌표계 1
library(lattice)
df_rate <- fread("C:/BigPublic/data/gimhae/08_arrest_rate.csv", header = T, encoding = "UTF-8", data.table = F)
row.names(df_rate) <- df_rate[[1]]
df_rate <- df_rate[-1]
pairs.panels(df_rate)
df_rate<-data.frame(df_rate[1], df_rate[3], df_rate[5], df_rate[6], df_rate[2], df_rate[4])

# 평행좌표계 그리기
parallelplot(df_rate, horizontal.axis=FALSE)

# 평행좌표계 2
df_year <- as.data.frame(t(df_rate))

# 평행좌표계 그리기
parallelplot(df_year, horizontal.axis=FALSE)



##########################################
## 연도별 범죄발생 현황 분석 위한 관계시각화
# 1. 막대그래프 그리기
barplot(df_1.crim$thief, names.arg = df_1.crim$year,
        col = "red",
        border = NA,
        xlab = "crime",
        ylab = colnames(df_1.crim)[[4]],
        main = "gimhae crime 2010~2015")

ggplot(df_1.crim, aes(x=df_1.crim$year, y=df_1.crim$violence))+geom_bar(stat="identity")

# 2. 누적막대그래프 그리기
df_1.crim.t <- data.frame(t(df_1.crim))
names(df_1.crim.t) <- df_1.crim.t[1,]
df_1.crim.t <- df_1.crim.t[-1,]

# 데이터 보기
df_1.crim.t
# 로우네임 보기
rownames(df_1.crim.t)
# 컬럼 추가하기
df_1.crim.t$crime_name <- rownames(df_1.crim.t)
# 데이터 보기
df_1.crim.t$crime_name
# 데이터값 기준으로 다시 불러오기
df_1.crim.t.melt <- melt(df_1.crim.t, id.vars="crime_name")
# 데이터 보기
head(df_1.crim.t.melt)

# 누적 막대 그래프 그리기
bar<-ggplot(df_1.crim.t.melt, aes(x=crime_name, y=value, fill=variable))
bar+geom_bar(stat="identity")


##########################################
## 경찰수와 범죄 발생 간의 상관성 시각화 - 산점도 

# 데이터 불러오기 
df_5.police <-fread("C:/BigPublic/data/gimhae/05_gimhae_policeman.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)

# scatterplot
library(corrplot)
df_crime_police <- merge(df_1.crim, df_5.police)

# 산점도 그리기 1
plot(df_crime_police[,13:19], main="Scatter Plot Matrix")

# 산점도 그리기 2
ggplot(data=df_crime_police, aes(x=gambling, y=policeman)) + geom_point(shape=10, size=5, colour="red") + ggtitle("Scatter Plot: gambling vs policeman")


##########################################
###3. 관계시각화를 통한 인자간 상관성 도출

# bubble chart1
library(corrplot)

df_3.cctv <- fread("C:/BigPublic/data/gimhae/03_gimhae_CCTV.csv", encoding = "UTF-8", header = TRUE, data.table = FALSE)

# 데이터 translation
df_crime_cctv <- merge(df_1.crim, df_3.cctv)

# 버블 차트 그리기
symbols(df_crime_cctv$year, df_crime_cctv$cctv_life,
        circles = df_crime_cctv$violence,
        inches = 0.5,
        fg = "white",
        bg = "blue",
        xlab = "연도",  ylab = "생활안전CCTV ",
        main = "연도별 생활안전 CCTV와 폭력범죄의 상관 관계" )


# bubble chart 2
library(ggplot2)
ggplot(df_crime_cctv, aes(x=year, y=cctv_life)) +
  geom_point(aes(size=violence), shape=21, colour="grey90", fill="blue", alpha=0.5) +
  scale_size_area(max_size = 15) + # 범례 없애려면 guide=FALSE
  geom_text(aes(y=cctv_life, label=violence),
            vjust=1, colour="grey40", size=3) +
  ggtitle("연도별 생활안전 CCTV와 폭력범죄의 상관 관계")

# bubble chart 3
library(plotly)
p <- plot_ly(df_crime_cctv, x = ~year, y = ~cctv_life, text = ~cctv_life, type = 'scatter', mode = 'markers',
             marker = list(size = ~10*log(violence), opacity = 0.5))
p

##########################################
###4. 경찰 수와 범죄유형 간의 상관계수 및 corrplot 그리기 

# 상관계수 구하기
crime_cor <- cor(df_crime_police[,2:19])

# 상관계수 확인
crime_cor

# 상관계수 그래프 그리기
library(corrplot)
corrplot(crime_cor, method="circle")

##########################################
###5. (참고:교재에는 없는 부분입니다.)

# 데이터 병합하기
df_crime_kara <- merge(df_1.crim, df_2.kara)
# 상관계수 구하기 & 상관계수 그래프 그리기
corrplot(cor(df_crime_kara), method="circle")

#-------------------------------------------
# 데이터 병합하기
df_crime_cctv <- merge(df_1.crim, df_3.cctv)
# 상관계수 구하기 & 상관계수 그래프 그리기
corrplot(cor(df_crime_cctv), method="circle")

# 데이터 병합하기
df_crime_police <- merge(df_1.crim, df_5.police)
# 상관계수 구하기 & 상관계수 그래프 그리기
df_crime_police_visitor <- merge(df_crime_police, df_6.visitor)

# 상관계수 구하기
crime_cor <- cor(df_crime_police_visitor[,2:22])
# 상관계수 확인
crime_cor
# 상관계수 그래프 그리기
corrplot(crime_cor, method="circle")
#-------------------------------------------

# 데이터 병합하기
df_all <- merge(df_1.crim, df_2.kara)
df_all <- merge(df_all, df_3.cctv)
df_all <- merge(df_all, df_4.foreign)
df_all <- merge(df_all, df_5.police)
df_all <- merge(df_all, df_6.visitor)

# 상관계수 구하기 & 상관계수 그래프 그리기
corrplot(cor(df_all), method="circle")

##########################################
###6. 범죄(폭력)와 CCTV 설치 간의 상관관계 

# 데이터 병합하기
df_all <- merge(df_1.crim, df_3.cctv) 

# 상관계수 구하기 & 상관계수 그래프 그리기
corrplot(cor(df_all), method="circle")

# 회귀모델 만들기
lm_out <- lm(violence ~ cctv_life , data=df_all)
# 회귀모델 정보 보기
summary(lm_out)

# 산점도 그리기 1
plot(violence ~ cctv_life,data=df_all)
# 회귀선 그리기
abline(lm_out,col="red")

# 산점도 + 회귀선 그리기
scatterplot(violence ~ cctv_life, reg.line=FALSE, smooth=FALSE, spread=FALSE,
            boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9),
            data=df_all)


