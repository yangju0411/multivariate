#7.1
library(MASS)
library(xlsx)
company.data = read.xlsx("exdata/ex7_1.xlsx", sheetIndex = 1)
head(company.data)
#Y=1�� ������, 2�� �������
company.lda = lda(Y~., data = company.data)
company.lda
# �������� ����Ȯ�� : 44.8%
# ��������� ����Ȯ�� : 55.2%

company.pred = predict(company.lda, newdata = company.data)
company.confm = table(company.data$Y, company.pred$class)
company.confm
# �������ε� ����������� �з��� ���� 1��
# ��������ε� ���������� �з��� ���� 2���̴�.

error = 1-sum(diag(company.confm))/sum(company.confm)
error
# �������� 10.34%�̴�.

library(klaR)
company.fwd = greedy.wilks(Y~.,data = company.data, niveau = 0.05)
company.fwd
#���Ǽ��� 0.05�Ͽ� �Ǹż���/���ڻ�(X2), �����Ѽҵ�/���ڻ�(X3)�� ���Ҵ�.
company.fwd.lda = lda(company.fwd$formula, data = company.data)
company.fwd.pred = predict(company.fwd.lda, newdata = company.data)
company.fwd.confm = table(company.data$Y, company.fwd.pred$class)
company.fwd.confm
# �������ε� ����������� �з��� ���� 3��
# ��������ε� ���������� �з��� ���� 0���̴�.

error = 1-sum(diag(company.fwd.confm))/sum(company.fwd.confm)
error
# �������� 10.34%�̴�.
# ���� ���� ���� ���� �������� ������ ���� �İ� ������ ���� ���� �� ����.
# �ٸ� �������� ã�Ƴ��� ���� �ָ����� ��� �������� ����������� �Ǵ��� Ȯ���� �� ���� ���� ���� ���� ���� �������� �ʴ�.
# ��������� �������̶�� �Ǵ��ϴ� �ͺ��� �� �����ϱ� �����̴�.


# 7.2
city.data = read.xlsx("exdata/ex7_2.xlsx", sheetIndex = 1)
head(city.data)
# Y=0�̸� ��������, 1�̸� �������

city.lda = lda(Y~., data = city.data)
city.lda
# ���������� ����Ȯ�� : 50%
# ��������� ����Ȯ�� : 50%

city.pred = predict(city.lda, newdata = city.data)
city.confm = table(city.data$Y, city.pred$class)
city.confm
# ���������ε� ������÷� �з��� ���� 6��
# ��������ε� �������÷� �з��� ���� 6���̴�.

error = 1-sum(diag(city.confm))/sum(city.confm)
error
# �������� 40%�̴�.


# 7.3
company.data = read.xlsx("exdata/ex7_3.xlsx", sheetIndex = 1)
head(company.data)
#Y=1�� ������, 0�� �������� ���� �������
company.lda = lda(Y~., data = company.data)
company.lda
# �������� ����Ȯ�� : 50%
# ��������� ����Ȯ�� : 50%

company.pred = predict(company.lda, newdata = company.data)
company.confm = table(company.data$Y, company.pred$class)
company.confm
# ��������ε� ���������� �з��� ���� 2��
# �������ε� ���������� �з��� ���� 1���̴�.

error = 1-sum(diag(company.confm))/sum(company.confm)
error
# �������� 7.9%�̴�.

company.fwd = greedy.wilks(Y~.,data = company.data, niveau = 0.05)
company.fwd
#���Ǽ��� 0.05�Ͽ� �� ��ä�� ���� ������ ����(X1), �� ��ä�� ���� �� �ڻ� ����(X3)�� ���Ҵ�.
company.fwd.lda = lda(company.fwd$formula, data = company.data)
company.fwd.pred = predict(company.fwd.lda, newdata = company.data)
company.fwd.confm = table(company.data$Y, company.fwd.pred$class)
company.fwd.confm
# �������ε� ����������� �з��� ���� 2��
# ��������ε� ���������� �з��� ���� 2���̴�.

error = 1-sum(diag(company.fwd.confm))/sum(company.fwd.confm)
error
# �������� 10.52%�̴�.
# �������� ����������� �Ǵ��� Ȯ���� �� �������� ������ ���� ���� ���� ���� �������� �ʴ�.
company.new = read.xlsx("exdata/ex7_3_2.xlsx", sheetIndex = 1)
company.new.pred = predict(company.lda, newdata = company.new)
company.new.pred$class
# ���� ����� ù��° ȸ�� �ϳ��̴�.