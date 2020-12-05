#7.1
library(MASS)
library(xlsx)
company.data = read.xlsx("exdata/ex7_1.xlsx", sheetIndex = 1)
head(company.data)
#Y=1이 도산기업, 2가 정상운영기업
company.lda = lda(Y~., data = company.data)
company.lda
# 도산기업의 사전확률 : 44.8%
# 정상운영기업의 사전확률 : 55.2%

company.pred = predict(company.lda, newdata = company.data)
company.confm = table(company.data$Y, company.pred$class)
company.confm
# 도산기업인데 정상운영기업으로 분류된 것은 1개
# 정상운영기업인데 도산기업으로 분류된 것은 2개이다.

error = 1-sum(diag(company.confm))/sum(company.confm)
error
# 에러율은 10.34%이다.

library(klaR)
company.fwd = greedy.wilks(Y~.,data = company.data, niveau = 0.05)
company.fwd
#유의수준 0.05하에 판매수익/총자산(X2), 세전총소득/총자산(X3)만 남았다.
company.fwd.lda = lda(company.fwd$formula, data = company.data)
company.fwd.pred = predict(company.fwd.lda, newdata = company.data)
company.fwd.confm = table(company.data$Y, company.fwd.pred$class)
company.fwd.confm
# 도산기업인데 정상운영기업으로 분류된 것은 3개
# 정상운영기업인데 도산기업으로 분류된 것은 0개이다.

error = 1-sum(diag(company.fwd.confm))/sum(company.fwd.confm)
error
# 에러율은 10.34%이다.
# 변수 선택 전과 후의 에러율은 같으나 선택 후가 변수의 수가 적어 더 좋다.
# 다만 도산기업을 찾아내는 것이 주목적일 경우 도산기업을 정상운영기업으로 판단할 확률이 더 높은 변수 선택 후의 모델은 적합하지 않다.
# 정상운영기업을 도산기업이라고 판단하는 것보다 더 위험하기 때문이다.


# 7.2
city.data = read.xlsx("exdata/ex7_2.xlsx", sheetIndex = 1)
head(city.data)
# Y=0이면 공업도시, 1이면 상업도시

city.lda = lda(Y~., data = city.data)
city.lda
# 공업도시의 사전확률 : 50%
# 상업도시의 사전확률 : 50%

city.pred = predict(city.lda, newdata = city.data)
city.confm = table(city.data$Y, city.pred$class)
city.confm
# 공업도시인데 상업도시로 분류된 것은 6개
# 상업도시인데 공업도시로 분류된 것은 6개이다.

error = 1-sum(diag(city.confm))/sum(city.confm)
error
# 에러율은 40%이다.


# 7.3
company.data = read.xlsx("exdata/ex7_3.xlsx", sheetIndex = 1)
head(company.data)
#Y=1이 도산기업, 0이 도산하지 않은 정상운영기업
company.lda = lda(Y~., data = company.data)
company.lda
# 도산기업의 사전확률 : 50%
# 정상운영기업의 사전확률 : 50%

company.pred = predict(company.lda, newdata = company.data)
company.confm = table(company.data$Y, company.pred$class)
company.confm
# 정상운영기업인데 도산기업으로 분류된 것은 2개
# 도산기업인데 정상기업으로 분류된 것은 1개이다.

error = 1-sum(diag(company.confm))/sum(company.confm)
error
# 에러율은 7.9%이다.

company.fwd = greedy.wilks(Y~.,data = company.data, niveau = 0.05)
company.fwd
#유의수준 0.05하에 총 부채에 대한 유동성 비율(X1), 총 부채에 대한 현 자산 비율(X3)만 남았다.
company.fwd.lda = lda(company.fwd$formula, data = company.data)
company.fwd.pred = predict(company.fwd.lda, newdata = company.data)
company.fwd.confm = table(company.data$Y, company.fwd.pred$class)
company.fwd.confm
# 도산기업인데 정상운영기업으로 분류된 것은 2개
# 정상운영기업인데 도산기업으로 분류된 것은 2개이다.

error = 1-sum(diag(company.fwd.confm))/sum(company.fwd.confm)
error
# 에러율은 10.52%이다.
# 도산기업을 정상운영기업으로 판단할 확률이 더 높아졌기 때문에 변수 선택 후의 모델은 적합하지 않다.
company.new = read.xlsx("exdata/ex7_3_2.xlsx", sheetIndex = 1)
company.new.pred = predict(company.lda, newdata = company.new)
company.new.pred$class
# 도산 기업은 첫번째 회사 하나이다.
