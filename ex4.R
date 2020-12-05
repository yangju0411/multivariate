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
# �ִܿ������ ���忬��� ������ ���غ����� �� �������� �Ҽ��ϴ� ������ �޶����� �ȴ�.
# ��κ��� ù��° ������ ���ϴ� �ִܿ������ �ٸ��� ���忬����� �ٸ� �������� ����� ������ �ִ�.

# (3)
win.graph()
par(mfrow = c(2, 1))
plot(ent.hc1, hang = -1)
plot(ent.hc2, hang = -1)
ent.single
ent.complete
# �ִܿ����(k = 4)�� ������ ������ 15�� 1�� 1�� 1����
# �±����, �Ѿ�ȭ��, ������ڰ� ���� ���� ��������.
# ���忬���(k = 4)�� ������ ������ 8�� 5�� 1�� 4����
# 1���� 2�� �������� ��ǰ, ���� ���� ȸ����� �ַ� �ְ� 3�� 4���� �Ǽ��̳� Ȯ�� ���� �����̴�.

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

# �������� �̿��� �ִܿ������ ���Ϲ���, ���ٴ�, ����ڰ� ���� ���� ��������. 
# �±����, �Ѿ�ȭ��, ������ڰ� ���� ���� �������� ��Ŭ����Ȱ� �ٸ���.
# �������� �̿��� �ִܿ������ ���Ϲ���, ���ٴ�, ����ڰ� ���� ���� ��������.
# ���忬����� ������ ���� �ٸ���.

# (5)
kmc <- kmeans(ent.zdata, 4)
kmc

pairs(ent.zdata, col = kmc$cluster, pch = 16)
# �����ں���, �ڻ��Ѱ�, �� ������ ���� ����� 2�� ����,
# �����ְ��� ���� PER�� ���� �� 3�� ����
# �ٸ� ��ġ���� ���� ������ PER�� ���� �� 1�� ����
# ��� ��ġ�� ���� ���� ���� 4�� ������ �Ҽӵȴ�.


# 4.2
iris.data <- iris
head(iris)
# (1)
iris.zdata <- stdize(as.matrix(iris[, -5]))
iris.zdata
# ǰ�� �����ʹ� ��ġ�� �ƴϹǷ� �����Ѵ�.

# (2)
iris.kmc <- kmeans(iris.zdata, 3)
iris.kmc
pairs(iris.zdata, col = iris.kmc$cluster, pch = 16)

# (3)
table(iris.kmc$cluster, iris[, 5])
# 1�� �������� �ַ� versicolor ���� �Ҽӵǰ�
# 2�� �������� �ַ� virginica ���� �Ҽӵȴ�.
# 3�� �������� setosa�� �Ҽӵȴ�.
# 1���� 2������ ���� �������� �ٸ� ������ �Ҽӵ� �͵��� �����Ѵ�.
# setosa ���� �����ϴµ��� ������ �������� versicolor�� virginica���� ������ ���� �� �ϴ�.

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
# 6���� �������� ������ �� ���� �� �ϴ�.

# (3)
city.kmc <- kmeans(city.zdata, 4)
city.kmc
pairs(city.data, col = city.kmc$cluster, pch = 16)
# 1�� ������ �뵿�ð��� ������ ������ �۰� �ҵ��� ���� ���̴�.
# 2�� ������ ������ �ҵ��� ���� �뵿�ð��� ����.
# 3���� ��ü������ ��������.
# 4�� ������ �뵿�ð��� ������ ���� ������ �ҵ��� ����.