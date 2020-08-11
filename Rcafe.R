library('ggplot2')
library('gridExtra')
cafe <- read.csv('C:/bg/Rcafe/cafe.csv')

# age 변수 생성: 각 점포별 가장 많은 이용 고객 연령대
age = c('age10_rate', 'age20_rate', 'age30_rate', 'age40_rate', 'age50_rate', 'age60_rate')
for(i in 1:dim(cafe)[1]){
  for(n in 1:length(age)){
    if(cafe[i, age[n]] == max(cafe[i, age])) cafe[i, 'age'] <- 10*n
  }
}
cafe$age <- as.factor(cafe$age)

# 가장 많은 이용 연령층: 20-30대
ggplot(data=cafe, aes(x=age)) +
  geom_bar()

# time 변수 생성: 각 점포별 가장 많은 이용 고객 시간대
# time1(00~06) time2(06~11) time3(11~14) time4(14~17) time5(17~21) time6(21~24)
time = c('time1_rate', 'time2_rate', 'time3_rate', 'time4_rate', 'time5_rate', 'time6_rate')
for(i in 1:dim(cafe)[1]){
  for(n in 1:length(time)){
    if(cafe[i, time[n]] == max(cafe[i, time])) cafe[i, 'time'] <- paste0('time', n)
  }
}
cafe$time <- as.factor(cafe$time)

# 시간대별 이용 고객: 11~14시가 피크 타임
ggplot(data=cafe, aes(x=time)) +
  geom_bar()

# gender 변수 생성: 각 점포별 주 고객층의 성별(여:0, 남:1)
for (i in 1:dim(cafe)[1]){
  if(cafe[i, 'woman_rate'] > cafe[i, 'man_rate']) cafe[i, 'gender'] = 0
  else cafe[i, 'gender'] = 1
}
cafe$gender <- as.factor(cafe$gender)

# gender_chr 변수 생성: 남성, 여성
for (i in 1:dim(cafe)[1]){
  if(cafe[i, 'woman_rate'] > cafe[i, 'man_rate']) cafe[i, 'gender_chr'] = '여성'
  else cafe[i, 'gender_chr'] = '남성'
}

# 여자가 주 고객층인 카페가 2배 이상 많다.
ggplot(data=cafe, aes(x=gender_chr)) +
  geom_bar(fill=I("blue"), 
           col=I("red"), 
           alpha=I(.3)) +
  labs(x = '주 고객층 성별', y = '고객 수(명)')

# 남성이 주 고객층인 카페의 나이 비율 -> 주로 30대, 40대 이후로는 거의 없음, 노인은 여성보다 많음
# 여성이 주 고객층인 카페의 나이 비율-> 압도적으로 20대가 많음
ggplot( data = cafe, aes(x = age)) +
  geom_histogram(binwidth = 10, color= 'black', fill = 'pink') +
  facet_grid(~gender_chr) +
  labs( x = '주 연령층(대)', y = '고객 수(명)')

# price 변수 생성: 월매출/월건수 = 거래 한 건당 평균 가격
for (i in 1:dim(cafe)[1]){
  cafe[i, 'price'] <- cafe$sum[i] / cafe$sales[i]
}

# 최적의 가격 선정: 평균적으로 9천원 정도일 때 최대 매출
# cafe_new: 이상치 제거(건당 평균 결제 금액이 5만원 이하), 월매출 150억 이하
cafe_new <- cafe[(cafe$price < 50000) & (cafe$sum < 15000000000),]
ggplot(data = cafe_new, aes(x = price, y = sum)) +
  geom_point(alpha=.2, colour='dark green') +
  xlab('결제 한 건당 가격(원)') +
  ylab('매출(원)') +
  ggtitle('최적의 가격 선정') +
  stat_smooth(method=loess, level=0.95)

# 요일별 매출 추이
daysum <- c(mean(cafe_new$mon_sum), mean(cafe_new$tue_sum), mean(cafe_new$wed_sum), mean(cafe_new$thu_sum), mean(cafe_new$fri_sum), mean(cafe_new$sat_sum), mean(cafe_new$sun_sum))
daysales <- c(mean(cafe_new$mon_sales), mean(cafe_new$tue_sales), mean(cafe_new$wed_sales), mean(cafe_new$thu_sales), mean(cafe_new$fri_sales), mean(cafe_new$sat_sales), mean(cafe_new$sun_sales))

d <- c('월', '화', '수', '목', '금', '토', '일')
day <- factor(d, levels=d)
plot(x=day, y=daysum)
plot(x=day, y=daysales)

# v_price 변수 생성
# sum/sales = 1회당 결제 금액(price)
# 가능변수: 요일, 성별, 연령
vars <- c('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun', 'man', 'woman', 'age10', 'age20', 'age30', 'age40', 'age50', 'age60')
for(i in 1:dim(cafe)[1]){
  for(v in vars){
    sum_v <- paste0(v, '_sum')
    sales_v <- paste0(v, '_sales')
    price_v <- paste0(v, '_price')
    cafe[i, price_v] <- cafe[i, sum_v]/cafe[i, sales_v]
  }
}
cafe <- subset(cafe, select=-vars)
cafe_new <- cafe[(cafe$price < 50000) & (cafe$sum < 15000000000),]

head(cafe)

# price 그래프
# 1. 요일별(3만원 이하)
par(mfrow=c(1, 7))
boxplot(cafe_new$mon_price, ylim=c(0,30000))
boxplot(cafe_new$tue_price, ylim=c(0,30000))
boxplot(cafe_new$wed_price, ylim=c(0,30000))
boxplot(cafe_new$thu_price, ylim=c(0,30000))
boxplot(cafe_new$fri_price, ylim=c(0,30000))
boxplot(cafe_new$sat_price, ylim=c(0,30000))
boxplot(cafe_new$sun_price, ylim=c(0,30000))

# 2. 성별(6만원 이하)
par(mfrow=c(1, 2))
boxplot(cafe_new$man_price, ylim=c(0,60000), xlab='남성')
boxplot(cafe_new$woman_price, ylim=c(0,60000), xlab='여성')

# 3. 연령(5만원 이하)
par(mfrow=c(1, 6))
boxplot(cafe_new$age10_price, ylim=c(0,50000), xlab='10대')
boxplot(cafe_new$age20_price, ylim=c(0,50000), xlab='20대')
boxplot(cafe_new$age30_price, ylim=c(0,50000), xlab='30대')
boxplot(cafe_new$age40_price, ylim=c(0,50000), xlab='40대')
boxplot(cafe_new$age50_price, ylim=c(0,50000), xlab='50대')
boxplot(cafe_new$age60_price, ylim=c(0,50000), xlab='60대 이상')

cafe_female<-cafe[cafe['gender']==0,]
cafe_male<-cafe[cafe['gender']==1,]

# 여심저격, 남심저격 카페의 연령대별 이용건수*건당 평균 가격(연령대별 매출)
par(mfrow=c(1,2))

a1=mean(cafe_female$age10_price, na.rm=T)*dim(cafe_female[cafe_female$age==10,])[1]
a2=mean(cafe_female$age20_price, na.rm=T)*dim(cafe_female[cafe_female$age==20,])[1]
a3=mean(cafe_female$age30_price, na.rm=T)*dim(cafe_female[cafe_female$age==30,])[1]
a4=mean(cafe_female$age40_price, na.rm=T)*dim(cafe_female[cafe_female$age==40,])[1]
a5=mean(cafe_female$age50_price, na.rm=T)*dim(cafe_female[cafe_female$age==50,])[1]
a6=mean(cafe_female$age60_price, na.rm=T)*dim(cafe_female[cafe_female$age==60,])[1]
a<-c(a1,a2,a3,a4,a5,a6)

b1=mean(cafe_male$age10_price, na.rm=T)*dim(cafe_male[cafe_male$age==10,])[1]
b2=mean(cafe_male$age20_price, na.rm=T)*dim(cafe_male[cafe_male$age==20,])[1]
b3=mean(cafe_male$age30_price, na.rm=T)*dim(cafe_male[cafe_male$age==30,])[1]
b4=mean(cafe_male$age40_price, na.rm=T)*dim(cafe_male[cafe_male$age==40,])[1]
b5=mean(cafe_male$age50_price, na.rm=T)*dim(cafe_male[cafe_male$age==50,])[1]
b6=mean(cafe_male$age60_price, na.rm=T)*dim(cafe_male[cafe_male$age==60,])[1]
b<-c(b1,b2,b3,b4,b5,b6)

plot(x=seq(10, 60, 10), a, type='b', main='여성', xlab='연령대', ylab='매출')
plot(x=seq(10, 60, 10), b, type='b', main='남성', xlab='연령대', ylab='매출', ylim=c(0,5000000))

# 남성, 여성 주력 카페의 이용 연령층
ggplot( data = cafe, aes(x = age)) +
  geom_histogram(binwidth = 10, color= 'black', fill = 'pink') +
  facet_grid(~gender_chr) +
  labs( x = '주 연령층(대)', y = '고객 수(명)')

# 평일, 주말 비교
options(scipen=100)
a = 
  ggplot(data = cafe_new, aes(x = age, y = weekday_sum/100000000/5)) + 
  ylim(c(0, 40)) +
  geom_point(size=4, alpha=.1, color='dark red') + 
  labs( x = '주 연령층(대)', y='평일 매출(억 원)')+
  facet_grid(~gender_chr)
b = 
  ggplot(data = cafe_new, aes(x = age, y = weekend_sum/100000000/2)) + 
  geom_point(size=4,alpha=.1, color='dark blue') + 
  labs( x = '주 연령층(대)', y='주말 매출(억 원)')+
  facet_grid(~gender_chr)
grid.arrange(a,b, nrow=2, ncol=1)

#write.csv(cafe, 'C:/bg/Rcafe/cafe.csv', row.names=F)
#write.csv(cafe_new, 'C:/bg/Rcafe/cafe_new.csv', row.names=F)