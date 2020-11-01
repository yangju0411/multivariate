# 라이브러리 불러오기
library(psych)
library(GPArotation)

# 데이터 불러오기
gangnam.data = read.table("exdata/ex3-1.txt", row.names = 1)
colnames(gangnam.data) = c(paste0(rep("x", 4), 1:4, ""), paste0(rep("y", 4), 1:4, ""))
# 체력진단 테스트는 x, 운동능력테스트는 y로 이름 정했음
# 초가 낮을 수록 좋은 50m달리기(y1)은 변환이 필요하다.
gangnam.data$y1 = max(gangnam.data$y1) - gangnam.data$y1
round(cor(gangnam.data), 2)
# 상관계수를 봄
# 멀리뛰기(y2)에 관련된 근육으로 알려진 배근력(x3)과 멀리뛰기의 양의 상관이 높다.
# 수직으로 높이뛰기(x2)도 마찬가지로 멀리뛰기와 상관관계가 높다.
# x2와 턱걸이(y4)의 상관관계도 높다.
# 50m 달리기는 전체적으로 상관이 존재한다.

gangnam.factor = principal(gangnam.data, rotate = "none")
gangnam.factor$values
# 고유값이 1을 넘는 것은 2개이다.
plot(gangnam.factor$values, type = "b")
# 최종적으로 2개를 선택한다.
# varimax 회전한다.
gangnam.varimax = principal(gangnam.data, nfactors = 2, rotate = "varimax", scores = T)
gangnam.varimax
# h2(공통성)은 모든 변수들이 높다.
# RC1에 대해 허리굽혀펴기(x1), 볼던지기(y3)를 제외한 다른 변수들의 부하가 높다.
# RC2에 대해서는 x1과 y3가 부하가 높고 악력(x4) 하나만 음수 값을 가진다.
# RC1은 39% RC2는 20%의 설명력을 가진다.

biplot(gangnam.varimax)
# y3, x1이 비슷한 벡터를 가지고 그 외 나머지 변수들이 비슷한 벡터를 가진다. x4는 RC2에서 음수를 가졌던만큼 다른 변수들과 방향이 다르다.

gangnam.varimax$scores
#x1= 0.26f1 + 0.80f2 + e1
#x2= 0.67f1 + 0.29f2 + e2
#x3= 0.75f1 + 0.01f2 + e3
#x4= 0.53f1 - 0.41f2 + e4
#y1= 0.71f1 + 0.12f2 + e5
#y2= 0.84f1 + 0.20f2 + e6
#y3= 0.11f1 + 0.77f2 + e7
#y4= 0.72f1 + 0.21f2 + e8
# 위 식으로 계산된 팩터의 스코어이다.

