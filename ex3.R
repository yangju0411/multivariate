# ex 3-1
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
#f1= 0.26x1 + 0.67x2 + 0.75x3 + 0.53x4 + 0.71y1 + 0.84y2 + 0.11y3 + 0.72y4
#f2= 0.8x1 + 0.29x2 - 0.41x4 + 0.12y1 + 0.2y2 + 0.77y3 + 0.21y4
# 위 식으로 계산된 팩터의 스코어이다.


# ex3-2
# 데이터 불러오기
subject = read.csv("mvadata/favoritesujects.csv", row.names = 1)
head(subject)
# (1)
subject.fact = principal(subject, nfactors = 6, rotate = "none")
subject.fact
plot(subject.fact$values, type = "b")
# 고유값의 크기와 누적 분산, 스크리 플롯의 기울기를 보았을 때 적절한 인자의 수는 두 개이다.
# 두 개의 인자가 획득하는 정보의 양은 76%이다.

#(2)
subject.fact = principal(subject, nfactors = 2, rotate = "none", scores = T)
subject.varimax = principal(subject, nfactors = 2, rotate = "varimax", scores = T)
subject.promax = principal(subject, nfactors = 2, rotate = "promax", scores = T)

subject.fact$loadings
# f1(BIO, GEO, CHEM, ALG, CALC, STAT)
# f2(-BIO, -GEO, -CHEM, ALG, CALC, STAT)
# f1의 고유값은 2.787, f2는 1.778 누적 분산의 비율은 0.761이다.
subject.varimax$loadings
# f1(BIO, GEO, CHEM)
# f2(ALG, CALC, STAT)
# f1의 고유값은 2.406, f2는 2.158 누적 분산의 비율은 0.761이다.
subject.promax$loadings
# f1(BIO, GEO, CHEM)
# f2(ALG, CALC, STAT)
# f1의 고유값은 2.437, f2는 2.146 누적 분산의 비율은 0.764이다.
# 종합적으로 보았을 때 각 부하 값은 변하지만 누적 분산 비율은 변하지 않는다.

# (3)
subject.varimax$loadings
# f1(BIO, GEO, CHEM) 생명, 지구, 화학으로 과학 Science로 나타낼 수 있다.
# f2(ALG, CALC, STAT) 대수, 미적분, 통계로 수학 Mathematics로 나타낼 수 있다.

# (4)
subject.varimax
# f1(BIO, GEO, CHEM) 생명, 지구, 화학으로 과학 Science로 나타낼 수 있다.
# f2(ALG, CALC, STAT) 대수, 미적분, 통계로 수학 Mathematics로 나타낼 수 있다.
# 공통성은 stat통계가 0.52로 가장 떨어진다.
# 총 나타내어지는 설명력은 76%이다.

subject.varimax$scores
# f1 = 0.9BIO + 0.86GEO + 0.9CHEM + 0.17STAT
# f2 = 0.122BIO + 0.145GEO + 0.889ALG + 0.914CALC + 0.702STAT

biplot(subject.varimax)
# 수학 과목군과 과학 과목군 들의 벡터의 방향이 서로 다르다.


# ex3.3
#데이터 불러오기
beer = read.csv("mvadata/beer(2).csv")
beer

#(1)
beer.fact0 = principal(beer, nfactors = 4, rotate = "none")
beer.fact0
plot(beer.fact0$values, type = "b")
# 고유값의 크기가 1 이상인 것, 누적 분산, 스크리플롯의 기울기를 보았을 때 적절한 인자의 수는 2개이다.
# 인자 두 개의 총 정보량은 98%이다.

#(2)
beer.fact = principal(beer, nfactors = 2, rotate = "none", scores = T)
beer.fact$loadings
# 인자 부하 행렬은 위와 같다.
# f1(CALORIES, ALCOHOL)
# f2(-SODIUM, COST)

beer.varimax = principal(beer, nfactors = 2, rotate = "varimax", scores = T)
beer.varimax$loadings
# 직교 회전 varimax의 인자 부하 행렬은 위와 같다. 누적 정보량은 같다.
# f1(CALORIES, ALCOHOL)
# f2(-SODIUM, COST)

beer.oblimin = principal(beer, nfactors = 2, rotate = "oblimin", scores = T)
beer.oblimin$loadings
# 사각 회전 oblimin의 인자 부하 행렬은 위와 같다. 누적 정보량은 같은 것으로 보아 직각에 가깝게 회전했다.
# f1(CALORIES, ALCOHOL)
# f2(-SODIUM, COST)

# (3)
beer.varimax$loadings
# f1(CALORIES, ALCOHOL) 12온스라는 정해진 분량에서 열량과 도수의 부하가 높은 것으로 보아 이 인자의 이름은 가성비 performance 정도로 볼 수 있을 것 같다.
# f2(-SODIUM, COST) 나트륨 함량이 음수로 부하가 높고 가격이 양수로 부하가 높다. 저염량 low sodium정도로 생각할 수 있다.

# (4)
beer.varimax
# f1(CALORIES, ALCOHOL) 12온스라는 정해진 분량에서 열량과 도수의 부하가 높은 것으로 보아 이 인자의 이름은 가성비 performance 정도로 볼 수 있을 것 같다.
# f2(-SODIUM, COST) 나트륨 함량이 음수로 부하가 높고 가격이 양수로 부하가 높다. 저염량 low sodium정도로 생각할 수 있다.
# 공통성이 모두 높다.
# 총 나타내어지는 설명력은 92%이다.

beer.varimax$scores
# f1 = 0.98cal + 0.43sodium + 0.96alcohol + 0.38cost
# f2 = -0.84sodium + 0.863cost

biplot(beer.varimax)
# 알코올 도수와 열량의 벡터의 방향이 비슷하고 가격과 나트륨은 거의 정 반대이다.
