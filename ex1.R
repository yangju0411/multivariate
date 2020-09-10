#1.1
# 데이터 생성
convinience.v <- c(70.5, 64.8,67.1,61.1,63.4,72.3,64.2,68.4,66.1,63.5,69,63.2,64.1)
accuracy.v <- c(59.4, 70.3,79.6,65,66.5,69.1,72,67.5,66.5,65.7,74.3,65.5,64.8)
kindness.v <- c(63.7, 68.6, 78.5,65.6,67.9,74.2,71.4,67.3,67.3,64.3,80.5,68.3,67.8)
efficiency.v <- c(54.3, 55.2, 62.4,54.4,65,60,56.9,51.3,50.7,53.9,63.6,49.8,59.7)
pleasant.v <- c(66.9, 68, 79.8, 64.5, 59.7,70.1,72.8,71.3,63.4,61.7,75.7,64.6,65.7)
automatic.v <- c(62.6, 64.1, 62.4,63.9,62,68.2,57.8,65.8,63.3,62.7,55.9,59.1,61.8)
rows = c("kookmin", "enterpr", "boram", "commerce", "seoul", "shinhan", "city",
         "exchange", "first", "chohung", "hana", "hanil", "house")
cols = c("convinience", "accuracy", "kindness", "efficiency", "pleasant", "automatic")
bank <- data.frame(convinience.v, accuracy.v, kindness.v, efficiency.v, pleasant.v, automatic.v, row.names = rows)
colnames(bank) <- cols

#(1)
attach(bank)
hist(convinience)
# 오른쪽으로 꼬리가 늘어진 것을 보아 왜도가 양수일 것으로 생각된다.
hist(accuracy)
# 살짝 대칭적이고 가운데가 솟아 있다. 왜도가 0에 가깝고 첨도가 높을 것으로 예상된다.
hist(kindness)
# 오른쪽으로 꼬리가 있고 평균부분이 솟은 모양으로 왜도가 양수이고 첨도가 높을 것으로 예상된다.
hist(efficiency)
# 오른쪽으로 꼬리가 있지만 전체적으로 균등한 모양이다.
hist(pleasant)
# 오른쪽으로 꼬리가 있다
hist(automatic)
# 왼쪽으로 꼬리가 있고 첨도가 높으며 오른쪽에 이상값이 존재한다.

#(2)
plot(bank)
cor(bank)
# accuracy(신속성)와 kindness(친절)이 강한 양의 상관관계를 가진다.
# accuracy(신속성)와 efficiency(능률)이 약한 양의 상관관계
# accuracy(신속성)와 pleasant(쾌적)는 강한 양의 상관관계가 있다.
# kindness(친절)와 efficiency(능률)이 양의 상관관계를 보인다.
# kindness(친절)와 pleasant(쾌적)가 강한 양의 상관관계를 보인다.
# kindness(친절)와 convinience(편리성)가 약한 양의 상관관계를 보인다.

#(3)
library(aplpack)
stars(bank)
faces(bank)
# boram과 shinhan이 전체적으로 우수하며 hanil이 낮은 평가를 받고 있다.

#----------------------------------------------------------------------------
# 1.2
# 데이터 생성
charity <- matrix(c(5.4, 3.1, 3.5, 5.7, 8.6, 25, 20.4, 26, 22, 36.3, 34.1, 28, 14.4, 11.4,  4.5),
                  ncol = 3, byrow = T)
rownames(charity) <- paste(rep("T", 5), 1:5, sep = "")
colnames(charity) <- paste(rep("C", 3), 1:3, sep = "")
charity

#(1)
barplot(charity)
#(2)
pie(charity[, "C1"])

#-------------------------------------------------------------

# 1.3
#(1)
tdist <- rt(100, 5)

#(2)
hist(tdist)
# 왼쪽으로 꼬리를 가지며 이상값이 존재한다. 

#(3)
boxplot(tdist)
# 이상값이 존재한다.

#(4)
stem(tdist)
# 0줄기에 수가 제일 많이 분포하며 -4에 이상값을 지닌다.

#---------------------------------------------------------
#1.4
head(longley)
plot(longley)
stars(longley)
faces(longley)
# Unemployed와 Armed.Forces를 제외한 값들이 연도가 상승할 수록 상승하는 경향을 보인다.
# 취업율, 인구, 국민총생산 등이 상승하는 것이므로 경제가 꾸준히 성장함을 알 수 있다.

#--------------------------------------------------------------------------
#1.5
library(HSAUR2)
attach(USairpollution)
#(1)
plot(USairpollution)
stars(USairpollution)
faces(USairpollution)
# 필라델피아와 알바니가 오염이 가장 심하다.

#(2)
library(MVA)
bvbox(USairpollution[,c("temp", "wind")], xlab = "temp", ylab = "wind")
# wind 6과 9 temp 70이 넘는 이상값이 존재한다.

#(3)
plot(manu~popul)
symbols(manu~popul, circles = SO2, add = T)
# manu와 popul이 양의 선형적 관계를 보이며 매우 큰 대도시가 존재한다. 
# popul과 manu가 작아도 SO2가 높은 도시도 존재한다.