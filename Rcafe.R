library('ggplot2')
library('gridExtra')
cafe <- read.csv('C:/bg/Rcafe/cafe.csv')

# age ���� ����: �� ������ ���� ���� �̿� ���� ���ɴ�
age = c('age10_rate', 'age20_rate', 'age30_rate', 'age40_rate', 'age50_rate', 'age60_rate')
for(i in 1:dim(cafe)[1]){
  for(n in 1:length(age)){
    if(cafe[i, age[n]] == max(cafe[i, age])) cafe[i, 'age'] <- 10*n
  }
}
cafe$age <- as.factor(cafe$age)

# ���� ���� �̿� ������: 20-30��
ggplot(data=cafe, aes(x=age)) +
  geom_bar()

# time ���� ����: �� ������ ���� ���� �̿� ���� �ð���
# time1(00~06) time2(06~11) time3(11~14) time4(14~17) time5(17~21) time6(21~24)
time = c('time1_rate', 'time2_rate', 'time3_rate', 'time4_rate', 'time5_rate', 'time6_rate')
for(i in 1:dim(cafe)[1]){
  for(n in 1:length(time)){
    if(cafe[i, time[n]] == max(cafe[i, time])) cafe[i, 'time'] <- paste0('time', n)
  }
}
cafe$time <- as.factor(cafe$time)

# �ð��뺰 �̿� ����: 11~14�ð� ��ũ Ÿ��
ggplot(data=cafe, aes(x=time)) +
  geom_bar()

# gender ���� ����: �� ������ �� �������� ����(��:0, ��:1)
for (i in 1:dim(cafe)[1]){
  if(cafe[i, 'woman_rate'] > cafe[i, 'man_rate']) cafe[i, 'gender'] = 0
  else cafe[i, 'gender'] = 1
}
cafe$gender <- as.factor(cafe$gender)

# gender_chr ���� ����: ����, ����
for (i in 1:dim(cafe)[1]){
  if(cafe[i, 'woman_rate'] > cafe[i, 'man_rate']) cafe[i, 'gender_chr'] = '����'
  else cafe[i, 'gender_chr'] = '����'
}

# ���ڰ� �� �������� ī�䰡 2�� �̻� ����.
ggplot(data=cafe, aes(x=gender_chr)) +
  geom_bar(fill=I("blue"), 
           col=I("red"), 
           alpha=I(.3)) +
  labs(x = '�� ������ ����', y = '���� ��(��)')

# ������ �� �������� ī���� ���� ���� -> �ַ� 30��, 40�� ���ķδ� ���� ����, ������ �������� ����
# ������ �� �������� ī���� ���� ����-> �е������� 20�밡 ����
ggplot( data = cafe, aes(x = age)) +
  geom_histogram(binwidth = 10, color= 'black', fill = 'pink') +
  facet_grid(~gender_chr) +
  labs( x = '�� ������(��)', y = '���� ��(��)')

# price ���� ����: ������/���Ǽ� = �ŷ� �� �Ǵ� ��� ����
for (i in 1:dim(cafe)[1]){
  cafe[i, 'price'] <- cafe$sum[i] / cafe$sales[i]
}

# ������ ���� ����: ��������� 9õ�� ������ �� �ִ� ����
# cafe_new: �̻�ġ ����(�Ǵ� ��� ���� �ݾ��� 5���� ����), ������ 150�� ����
cafe_new <- cafe[(cafe$price < 50000) & (cafe$sum < 15000000000),]
ggplot(data = cafe_new, aes(x = price, y = sum)) +
  geom_point(alpha=.2, colour='dark green') +
  xlab('���� �� �Ǵ� ����(��)') +
  ylab('����(��)') +
  ggtitle('������ ���� ����') +
  stat_smooth(method=loess, level=0.95)

# ���Ϻ� ���� ����
daysum <- c(mean(cafe_new$mon_sum), mean(cafe_new$tue_sum), mean(cafe_new$wed_sum), mean(cafe_new$thu_sum), mean(cafe_new$fri_sum), mean(cafe_new$sat_sum), mean(cafe_new$sun_sum))
daysales <- c(mean(cafe_new$mon_sales), mean(cafe_new$tue_sales), mean(cafe_new$wed_sales), mean(cafe_new$thu_sales), mean(cafe_new$fri_sales), mean(cafe_new$sat_sales), mean(cafe_new$sun_sales))

d <- c('��', 'ȭ', '��', '��', '��', '��', '��')
day <- factor(d, levels=d)
plot(x=day, y=daysum)
plot(x=day, y=daysales)

# v_price ���� ����
# sum/sales = 1ȸ�� ���� �ݾ�(price)
# ���ɺ���: ����, ����, ����
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

# price �׷���
# 1. ���Ϻ�(3���� ����)
par(mfrow=c(1, 7))
boxplot(cafe_new$mon_price, ylim=c(0,30000))
boxplot(cafe_new$tue_price, ylim=c(0,30000))
boxplot(cafe_new$wed_price, ylim=c(0,30000))
boxplot(cafe_new$thu_price, ylim=c(0,30000))
boxplot(cafe_new$fri_price, ylim=c(0,30000))
boxplot(cafe_new$sat_price, ylim=c(0,30000))
boxplot(cafe_new$sun_price, ylim=c(0,30000))

# 2. ����(6���� ����)
par(mfrow=c(1, 2))
boxplot(cafe_new$man_price, ylim=c(0,60000), xlab='����')
boxplot(cafe_new$woman_price, ylim=c(0,60000), xlab='����')

# 3. ����(5���� ����)
par(mfrow=c(1, 6))
boxplot(cafe_new$age10_price, ylim=c(0,50000), xlab='10��')
boxplot(cafe_new$age20_price, ylim=c(0,50000), xlab='20��')
boxplot(cafe_new$age30_price, ylim=c(0,50000), xlab='30��')
boxplot(cafe_new$age40_price, ylim=c(0,50000), xlab='40��')
boxplot(cafe_new$age50_price, ylim=c(0,50000), xlab='50��')
boxplot(cafe_new$age60_price, ylim=c(0,50000), xlab='60�� �̻�')

cafe_female<-cafe[cafe['gender']==0,]
cafe_male<-cafe[cafe['gender']==1,]

# ��������, �������� ī���� ���ɴ뺰 �̿�Ǽ�*�Ǵ� ��� ����(���ɴ뺰 ����)
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

plot(x=seq(10, 60, 10), a, type='b', main='����', xlab='���ɴ�', ylab='����')
plot(x=seq(10, 60, 10), b, type='b', main='����', xlab='���ɴ�', ylab='����', ylim=c(0,5000000))

# ����, ���� �ַ� ī���� �̿� ������
ggplot( data = cafe, aes(x = age)) +
  geom_histogram(binwidth = 10, color= 'black', fill = 'pink') +
  facet_grid(~gender_chr) +
  labs( x = '�� ������(��)', y = '���� ��(��)')

# ����, �ָ� ��
options(scipen=100)
a = 
  ggplot(data = cafe_new, aes(x = age, y = weekday_sum/100000000/5)) + 
  ylim(c(0, 40)) +
  geom_point(size=4, alpha=.1, color='dark red') + 
  labs( x = '�� ������(��)', y='���� ����(�� ��)')+
  facet_grid(~gender_chr)
b = 
  ggplot(data = cafe_new, aes(x = age, y = weekend_sum/100000000/2)) + 
  geom_point(size=4,alpha=.1, color='dark blue') + 
  labs( x = '�� ������(��)', y='�ָ� ����(�� ��)')+
  facet_grid(~gender_chr)
grid.arrange(a,b, nrow=2, ncol=1)

#write.csv(cafe, 'C:/bg/Rcafe/cafe.csv', row.names=F)
#write.csv(cafe_new, 'C:/bg/Rcafe/cafe_new.csv', row.names=F)