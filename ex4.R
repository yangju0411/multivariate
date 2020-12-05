# ex4.1
ent.data <- read.table("exdata/ex4-1.txt", header = F, row.names = 1, fileEncoding = "CP949")
colnames(ent.data) <- paste0(rep("x", 5), 1:5, sep = "")
head(ent.data)

# (1)
library(pls)
ent.zdata <- stdize(as.matrix(ent.data))
ent.zdata

# (2)
ent.hc1 <- hclust(dist(ent.zdata), method = "single")
ent.single <- cutree(ent.hc1, k = 2:5)
ent.single
table(ent.single[, 1])
table(ent.single[, 2])
table(ent.single[, 3])
table(ent.single[, 4])

ent.hc2 <- hclust(dist(ent.zdata), method = "complete")
ent.complete <- cutree(ent.hc2, k = 2:5)
ent.complete
table(ent.complete[, 1])
table(ent.complete[, 2])
table(ent.complete[, 3])
table(ent.complete[, 4])
# 최단연결법과 최장연결법 양쪽을 비교해보았을 때 변수들이 소속하는 군집이 달라지게 된다.
# 대부분이 첫번째 군집에 속하는 최단연결법과 다르게 최장연결법은 다른 군집에도 골고루 나눠져 있다.

# (3)
win.graph()
par(mfrow = c(2, 1))
plot(ent.hc1, hang = -1)
plot(ent.hc2, hang = -1)
ent.single
ent.complete
# 최단연결법(k = 4)로 묶었을 때에는 15개 1개 1개 1개로
# 태광산업, 한양화학, 대우전자가 각각 따로 묶여진다.
# 최장연결법(k = 4)로 묶었을 때에는 8개 5개 1개 4개로
# 1번과 2번 군집에는 식품, 제약 관련 회사들이 주로 있고 3번 4번은 건설이나 확학 전자 계통이다.

# (4)
ent.hccr1 <- hclust(as.dist(cor(t(ent.data))), method = "single")
ent.hccr1
ent.cor.simple <- cutree(ent.hccr1, k = 2:5)
ent.cor.simple

ent.hccr2 <- hclust(as.dist(cor(t(ent.data))), method = "complete")
ent.hccr2
ent.cor.complete <- cutree(ent.hccr2, k = 2:5)
ent.cor.complete 

table(ent.cor.simple[, 3])
table(ent.cor.complete[, 3])

win.graph()
par(mfrow = c(2, 2))
plot(ent.hc1, hang = -1)
plot(ent.hc2, hang = -1)
plot(ent.hccr1, hang = -1)
plot(ent.hccr2, hang = -1)

# 상관계수를 이용한 최단연결법은 동일방직, 종근당, 녹십자가 각각 따로 묶여졌다. 
# 태광산업, 한양화학, 대우전자가 각각 따로 묶여졌던 유클리디안과 다르다.
# 상관계수를 이용한 최단연결법은 동일방직, 종근당, 녹십자가 각각 따로 묶여졌다.
# 최장연결법도 기존과 많이 다르다.

# (5)
kmc <- kmeans(ent.zdata, 4)
kmc

pairs(ent.zdata, col = kmc$cluster, pch = 16)
# 수권자본금, 자산총계, 총 매출이 높은 기업이 2번 군집,
# 최저주가가 높고 PER이 낮은 게 3번 군집
# 다른 수치들이 높지 않으며 PER이 높은 게 1번 군집
# 모든 수치가 높지 않은 것이 4번 군집에 소속된다.


# 4.2
iris.data <- iris
head(iris)
# (1)
iris.zdata <- stdize(as.matrix(iris[, -5]))
iris.zdata
# 품종 데이터는 수치가 아니므로 제외한다.

# (2)
iris.kmc <- kmeans(iris.zdata, 3)
iris.kmc
pairs(iris.zdata, col = iris.kmc$cluster, pch = 16)

# (3)
table(iris.kmc$cluster, iris[, 5])
# 1번 군집에는 주로 versicolor 종이 소속되고
# 2번 군집에는 주로 virginica 종이 소속된다.
# 3번 군집에는 setosa가 소속된다.
# 1번과 2번에서 종이 같음에도 다른 군집에 소속된 것들이 존재한다.
# setosa 종을 구별하는데는 성능이 좋겠지만 versicolor와 virginica에는 오류가 있을 듯 하다.

# 4.3
city.data <- read.csv("exdata/ex4-3.csv", header = F, row.names = 1)
colnames(city.data) <- paste0(rep("x", 3), 1:3, sep = "")
head(city.data)

# (1)
city.zdata <- stdize(as.matrix(city.data))

# (2)
city.hc1 <- hclust(dist(city.zdata), method = "complete")
win.graph()
plot(city.hc1, hang = -1)
# 6개의 군집으로 나누는 게 좋을 듯 하다.

# (3)
city.kmc <- kmeans(city.zdata, 4)
city.kmc
pairs(city.data, col = city.kmc$cluster, pch = 16)
# 1번 군집은 노동시간이 낮으며 물가가 작고 소득이 높은 편이다.
# 2번 군집은 물가와 소득이 높고 노동시간이 적다.
# 3번은 전체적으로 떨어진다.
# 4번 군집은 노동시간이 높은데 비해 물가와 소득이 낮다.