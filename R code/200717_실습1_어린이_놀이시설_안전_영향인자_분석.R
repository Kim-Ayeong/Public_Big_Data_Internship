##### 실습1 - 어린이 놀이시설 안전 영향인자 분석

######## 비교시각화 #######
###########################
### 시간대별 장소별 (어린이 안전사고 발생 현황)

# 데이터셋 업로드
accident_time_location<-read.csv("../data/child/1_1_accident_time_location.csv")
View(accident_time_location)
str(accident_time_location)

# R 최신버전은  read.csv()의 stringsAsFactors = F가 default라 아래 코드 필요없음 
accident_time_location$time <-as.character(accident_time_location$time) 

# star plot
stars(accident_time_location[,2:9],labels=accident_time_location[[1]])
 
# 나이팅게일
stars(accident_time_location[,2:9],labels=accident_time_location[[1]],flip.labels = F, draw.segments = T)

# heatmap
accident_time_location_matrix <- as.matrix((accident_time_location[,2:9]))
rownames(accident_time_location_matrix) <-accident_time_location[[1]]
heatmap(accident_time_location_matrix, Rowv=NA, col=brewer.pal(9,"Blues"), scale="column", margin=c(5,10))


######## 비교시각화 #######
###########################
### 놀이시설 장소별 (어린이 안전사고 발생 현황)

# 데이터셋 업로드
accident_location_time <-read.csv("../data/child/1_2_accident_location_time.csv")
View(accident_location_time)
accident_location_time = accident_location_time[-1]
str(accident_location_time)

# R 최신버전은  read.csv()의 stringsAsFactors = F가 default라 아래 코드 필요없음 
accident_location_time$time <-as.character(accident_location_time$time) 

# star chart
stars(accident_location_time[,2:12], labels = accident_location_time[[1]])

# 나이팅게일
stars(accident_location_time[,2:12], labels = accident_location_time[[1]], flip.labels = T, draw.segments = T)


######## 비교시각화 #######
###########################
### 놀이기구별 (어린이 안전사고 발생 현황 분석(Bar chart))

# 데이터셋 업로드
Rides_accident <- read.csv("../data/child/2_Rids_accident.csv")
View(Rides_accident)
str(Rides_accident) 

# R 최신버전은  read.csv()의 stringsAsFactors = F가 default라 아래 코드 필요없음 
Rides_accident$rids <-as.character(Rides_accident$rids) 

#Bar Chart 
barplot(Rides_accident$injured_count, names.arg = Rides_accident$rids) 


######## 비교시각화 #######
###########################
### 연령대별, 놀이기구별 (어린이 안전사고 발생 현황 분석)

# 데이터셋 업로드
age_rides_accident <-read.csv("../data/child/3_age_rides_accident.csv")
View(age_rides_accident)
str(age_rides_accident)

# R 최신버전은  read.csv()의 stringsAsFactors = F가 default라 아래 코드 필요없음 
age_rides_accident$rides <-as.character(age_rides_accident$rides) 

# star plot
stars(age_rides_accident[2:9],labels = age_rides_accident[[1]])

# 나이팅게일
stars(age_rides_accident[,2:9],labels = age_rides_accident[[1]], flip.labels = F, draw.segments = T)

# heatmap
age_rides_accident_matrix <- data.matrix(age_rides_accident[,2:9])
rownames(age_rides_accident_matrix) <- age_rides_accident[[1]]
heatmap(age_rides_accident_matrix,Rowv=NA, col=brewer.pal(7,"Greens"), scale="column", margin=c(5,10))  
heatmap(age_rides_accident_matrix,Rowv=NA, col=brewer.pal(9,"Blues" ), scale="column", margin=c(5,10))
 

######## 관계시각화 #######
###########################
### 지역별 놀이시설 수 분석을 위한 관계시각화 

# 데이터셋 업로드
region_accident <- read.csv("../data/child/5_region_accident.csv")
View(region_accident)
str(region_accident)

# R 최신버전은  read.csv()의 stringsAsFactors = F가 default라 아래 코드 필요없음 
region_accident$region <-as.character(region_accident$region) 

region_accident_matrix <-data.matrix(region_accident[,2:3]) 
 
barplot(region_accident$playgrounds_count, names.arg = region_accident$region)
barplot(region_accident$injure_count, names.arg = region_accident$region)


######## 관계시각화 #######
###########################
### 놀이시설 장소와 부상자 발생 간의 상관성 시각화 

# 데이터셋 업로드
location_accident <- read.csv("../data/child/6_location_accident.csv")
str(location_accident)
View(location_accident)

# R 최신버전은  read.csv()의 stringsAsFactors = F가 default라 아래 코드 필요없음 
location_accident$location <-as.character(location_accident$location) 

# bubble chart
symbols(location_accident$count, location_accident$injured, circles = location_accident$injured_rate, inches=0.3, fg="white", bg="red", xlab="놀이시설수", ylab="부상자수", main="놀이시설수와 부상자수 관계")
        
# bubble chart2
accident_rate = location_accident$injured/location_accident$count

ggplot(location_accident, aes(x=count, y=injured)) + 
  geom_point(aes(size=accident_rate), shape=21, colour="grey90", fill="blue", alpha=0.5) +
  scale_size_area(max_size = 15) + # 범례 없애려면 guide=FALSE
  geom_text(aes(y=as.numeric(injured)-sqrt(accident_rate)/10, label=location), 
            vjust=1, colour="grey40", size=3) + 
  ggtitle("놀이시설수와 부상자수 관계")


######## 관계시각화 #######
###########################
### 놀이시설 수와 부상자 발생 간의 상관성 해석 관계시각화 
 
#데이터셋 업로드
region_accident <- read.csv("../data/child/5_region_accident.csv")
View(region_accident)
str(region_accident)

# 형변환
region_accident$playgrounds_count <- as.integer(region_accident$playgrounds_count) 

# scatter plot
#scatterplot.matrix(region_accident[,2:3]) 대신 pairs 
pairs(region_accident[,2:3])
scatterplotMatrix(region_accident[,2:3])

#놀이시설 수와 부상자 발생 간의 상관계수 및 corrplot 그리기 
region_accident_cor <- cor(region_accident[,2:3])


#상관계수확인 
region_accident_cor

#circle 1
corrplot(region_accident_cor, method="circle")

#circle2
corrplot(region_accident_cor, method="circle",addCoef.col = "red")


######## 관계시각화 #######
###########################
### 놀이시설 위험도와수 사고 발생 간의 상관관계
 
# 데이터셋 업로드
child_playground <- read.csv("../data/child/child_playgroud.csv")

# 데이터 확인
View(child_playground)

# 선형회귀분석
lm_out <- lm(accident~Ride_risk, data=child_playground)

# 선형회귀식 요약 출력
summary(lm_out)

# 산점도1
plot(accident~Ride_risk,data=child_playground)

# 회귀선 그리기
abline(lm_out,col="red")

# 산점도2
scatterplot(accident~Ride_risk, reg.line=FALSE, smooth=FALSE, spread=FALSE, 
            boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), 
            data=child_playground)

# 회귀선 그리기
abline(lm_out,col="red")


