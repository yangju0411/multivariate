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
# 남편의 변수들의 선형 결합을 w, 아내 변수들의 선형결합을 v
# w와 v의 상관계수는 위와 같다

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
#w1은 배우자와의 사랑(x1, x2)에 있어서 음의 상관을 갖는다.
#w2는 배우자와의 우의(x3, x4)에 있어서 음의 상관을 갖는다.
#w3는 배우자와의 우의(x3, x4)와 양의 상관을 갖고 x1과는 음의 상관을 갖는다.
#w4는 배우자의 사랑과 양의 상관을 가진다.
#v1은 전체적으로 음의 상관을 갖는다.
#v2는 배우자가 자신을 어떻게 생각하는지(x2, x4)에 대해 음의 상관을 갖는다
#v3은 자신이 배우자를 사랑하는지에 대해 음의 상관 우의를 가지는지에 대해 양의 상관을 갖는다.
#v4는 우의에 대해 음의 상관 배우자가 자신을 사랑하는지에 대해 양의 상관을 갖는다.
couple.result$cor
#남편이 아내와 사랑이 높다고 느끼는 쪽이 아내쪽에서도 사랑과 우의가 높다.
#남편이 아내와 우의가 높다고 느끼는 쪽이 아내쪽에서 남편이 자신에게 사랑/우의를 높게 느낀다고 생각한다.

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
# w1과 v1의 상관계수가 매우 크다.

data.result$xcoef
# w1 = -0.78x1 -0.89x2
# w2 = -0.06x1 +0.04x2
data.result$ycoef
# v1 = -0.43y1 -0.67y2
# v2 = -1.03y1 +0.92y2

data.result$scores
# w1은 x 전체와 음의상관이 매우 크다
# v1은 y 전체와 음의 상관이 매우 크다
# x1, x2가 클 수록 y1, y2도 크다.

#6.3
library(pls)
data("nutrimouse")
X <- nutrimouse$lipid #쥐의 지방산에 관한 변수
head(X)
Y <- nutrimouse$gene #쥐의 유전자 발현 수준
head(Y)

matcor(X, Y)

x <- X[, 1:5]
y <- Y[, 1:5]

matcor(x, y)

nutrimouse.cca <- cc(x, y)

nutrimouse.cca$cor
# w1과 v1이 0.78로 강한 양의 상관 관계가 있다.

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
# w1은 x2와 음의상관이 매우 크다. 그 다음으로 x3와 음의 상관이 크다.
# v1은 y4와 음의 상관이 매우 크다
# x2, x3가 클 수록 y4도 크다.
# w2는 x2를 제외하고 전체적으로 상관의 절대값이 크다.
# v2는 y3, y4와 상관이 크다

# 문항신뢰도
library(xlsx)
nurse <- read.xlsx("exdata/문항신뢰도(간호사직무).xls", sheetIndex = 1)
nurse
pairs(nurse[, -1], panel = panel.smooth)
psych::alpha(nurse[,-1])
# 크론바흐 알파는 0.77, 표준화 알파는 0.78 신뢰구간은 (0.6, 0.93)이다.
# q1, q3, q4, q5는 제거하면 알파값이 낮아진다.
# q2, q6는 제거해도 알파가 떨어지지 않으며 표준화 알파는 증가한다.
# 문항2(업무기술 부분)과 문항6(한계 부분)은 역할 갈등의 측면에서 볼 때 제거해도 좋을 것 같다.