# 6.1
couple <- read.csv("exdata/ex6_1.csv")
#(1)
library(CCA)
library(GGally)
library(psych)
library(ggplot2)
husband = couple[, 1:4]
wife = couple[,5:8]
cor(husband)
cor(wife)

#(2)
matcor(husband, wife)$XYcor

#(3)
couple.result <- cc(husband, wife)

couple.result$cor
# ������ �������� ���� ������ w, �Ƴ� �������� ���������� v
# w�� v�� �������� ���� ����

couple.result$xcoef
#w1 = 0.25x1 -0.86x2 -2.77x3 + 2.67x4
#w2 = -1.06x1 + 0.57x2 - 0.93x3 - 0.44x4
#w3 = -1.39x1 + 0.78x2 + 0.61x3 + 0.13x4
#w4 = 0.1x1 + 1.1x2 - 3.05x3 + 3.41x4

couple.result$ycoef
#v1 = -0.85y1 -0.7y2 -1.74y3 + 0.82y4
#v2 = 0.44y1 - 0.63y2 - 1.71y3 -2.38y4
#v3 = -1.05y1 -0.12y2 + 2.5y3 -1.35y4
#v4 = -0.24y1 + 1.32y2 + 0.58y3 - 2.1y4

#(4)
couple.result$scores
#w1�� ����ڿ��� ���(x1, x2)�� �־ ���� ����� ���´�.
#w2�� ����ڿ��� ����(x3, x4)�� �־ ���� ����� ���´�.
#w3�� ����ڿ��� ����(x3, x4)�� ���� ����� ���� x1���� ���� ����� ���´�.
#w4�� ������� ����� ���� ����� ������.
#v1�� ��ü������ ���� ����� ���´�.
#v2�� ����ڰ� �ڽ��� ��� �����ϴ���(x2, x4)�� ���� ���� ����� ���´�
#v3�� �ڽ��� ����ڸ� ����ϴ����� ���� ���� ��� ���Ǹ� ���������� ���� ���� ����� ���´�.
#v4�� ���ǿ� ���� ���� ��� ����ڰ� �ڽ��� ����ϴ����� ���� ���� ����� ���´�.
couple.result$cor
#������ �Ƴ��� ����� ���ٰ� ������ ���� �Ƴ��ʿ����� ����� ���ǰ� ����.
#������ �Ƴ��� ���ǰ� ���ٰ� ������ ���� �Ƴ��ʿ��� ������ �ڽſ��� ���/���Ǹ� ���� �����ٰ� �����Ѵ�.

w1 <- couple.result$scores$xscores[, 1]
w2 <- couple.result$scores$xscores[, 2]
w3 <- couple.result$scores$xscores[, 3]
w4 <- couple.result$scores$xscores[, 4]
v1 <- couple.result$scores$yscores[, 1]
v2 <- couple.result$scores$yscores[, 2]
v3 <- couple.result$scores$yscores[, 3]
v4 <- couple.result$scores$yscores[, 4]
par(mfrow = c(2, 2))
plot(w1, v1, pch = 19)
plot(w2, v2, pch = 19)
plot(w3, v3, pch = 19)
plot(w4, v4, pch = 19)

#6.2
data <- read.csv("exdata/ex6_2.csv")
data.x = data[,c(1:2)]
data.y = data[,c(3:4)]

matcor(data.x, data.y)

data.result = cc(data.x, data.y)

data.result$cor
# w1�� v1�� �������� �ſ� ũ��.

data.result$xcoef
# w1 = -0.78x1 -0.89x2
# w2 = -0.06x1 +0.04x2
data.result$ycoef
# v1 = -0.43y1 -0.67y2
# v2 = -1.03y1 +0.92y2

data.result$scores
# w1�� x ��ü�� ���ǻ���� �ſ� ũ��
# v1�� y ��ü�� ���� ����� �ſ� ũ��
# x1, x2�� Ŭ ���� y1, y2�� ũ��.

#6.3
library(pls)
data("nutrimouse")
X <- nutrimouse$lipid #���� ����꿡 ���� ����
head(X)
Y <- nutrimouse$gene #���� ������ ���� ����
head(Y)

matcor(X, Y)

x <- X[, 1:5]
y <- Y[, 1:5]

matcor(x, y)

nutrimouse.cca <- cc(x, y)

nutrimouse.cca$cor
# w1�� v1�� 0.78�� ���� ���� ��� ���谡 �ִ�.

nutrimouse.cca$xcoef
# w1 = 0.36x1 -0.25x2-0.17x3-1.7x4-0.07x5
# w2 = 1.58x1 +0.20x2-0.55x3-1.44x4-0.37x5
# w3 = -1.22x1 +0.08x2+0.1x3+5.57x4-0.09x5
# w4 = -1.90x1 +0.20x2-0.83x3-5.62x4+0.29x5
# w5 = -4.92x1 -0.13x2+0.51x3+3.71x4+1.48x5
nutrimouse.cca$ycoef
# v1 = 2.36y1 +0.6y2+0.65y3-10.66y4+0.18y5
# v2 = -2.45y1 -0.06y2+5.19y3-4.02y4+13.77y5
# v3 = 1.86y1 -5.53y2-3.61y3 -0.79y4+11.41y5
# v4 = 7.9y1 +18.4y2-0.99y3+2.61y4-10.21y5
# v5 = -15.3y1 +12.29y2-0.44y3+1.5y4+2.4y5
nutrimouse.cca$scores
# w1�� x2�� ���ǻ���� �ſ� ũ��. �� �������� x3�� ���� ����� ũ��.
# v1�� y4�� ���� ����� �ſ� ũ��
# x2, x3�� Ŭ ���� y4�� ũ��.
# w2�� x2�� �����ϰ� ��ü������ ����� ���밪�� ũ��.
# v2�� y3, y4�� ����� ũ��

# ���׽ŷڵ�
library(xlsx)
nurse <- read.xlsx("exdata/���׽ŷڵ�(��ȣ������).xls", sheetIndex = 1)
nurse
pairs(nurse[, -1], panel = panel.smooth)
psych::alpha(nurse[,-1])
# ũ�й��� ���Ĵ� 0.77, ǥ��ȭ ���Ĵ� 0.78 �ŷڱ����� (0.6, 0.93)�̴�.
# q1, q3, q4, q5�� �����ϸ� ���İ��� ��������.
# q2, q6�� �����ص� ���İ� �������� ������ ǥ��ȭ ���Ĵ� �����Ѵ�.
# ����2(������� �κ�)�� ����6(�Ѱ� �κ�)�� ���� ������ ���鿡�� �� �� �����ص� ���� �� ����.