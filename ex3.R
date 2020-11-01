# ex 3-1
# ���̺귯�� �ҷ�����
library(psych)
library(GPArotation)

# ������ �ҷ�����
gangnam.data = read.table("exdata/ex3-1.txt", row.names = 1)
colnames(gangnam.data) = c(paste0(rep("x", 4), 1:4, ""), paste0(rep("y", 4), 1:4, ""))
# ü������ �׽�Ʈ�� x, ��ɷ��׽�Ʈ�� y�� �̸� ������
# �ʰ� ���� ���� ���� 50m�޸���(y1)�� ��ȯ�� �ʿ��ϴ�.
gangnam.data$y1 = max(gangnam.data$y1) - gangnam.data$y1
round(cor(gangnam.data), 2)
# �������� ��
# �ָ��ٱ�(y2)�� ���õ� �������� �˷��� ��ٷ�(x3)�� �ָ��ٱ��� ���� ����� ����.
# �������� ���̶ٱ�(x2)�� ���������� �ָ��ٱ�� ������谡 ����.
# x2�� �ΰ���(y4)�� ������赵 ����.
# 50m �޸���� ��ü������ ����� �����Ѵ�.

gangnam.factor = principal(gangnam.data, rotate = "none")
gangnam.factor$values
# �������� 1�� �Ѵ� ���� 2���̴�.
plot(gangnam.factor$values, type = "b")
# ���������� 2���� �����Ѵ�.
# varimax ȸ���Ѵ�.
gangnam.varimax = principal(gangnam.data, nfactors = 2, rotate = "varimax", scores = T)
gangnam.varimax
# h2(���뼺)�� ��� �������� ����.
# RC1�� ���� �㸮�������(x1), ��������(y3)�� ������ �ٸ� �������� ���ϰ� ����.
# RC2�� ���ؼ��� x1�� y3�� ���ϰ� ���� �Ƿ�(x4) �ϳ��� ���� ���� ������.
# RC1�� 39% RC2�� 20%�� �������� ������.

biplot(gangnam.varimax)
# y3, x1�� ����� ���͸� ������ �� �� ������ �������� ����� ���͸� ������. x4�� RC2���� ������ ��������ŭ �ٸ� ������� ������ �ٸ���.

gangnam.varimax$scores
#f1= 0.26x1 + 0.67x2 + 0.75x3 + 0.53x4 + 0.71y1 + 0.84y2 + 0.11y3 + 0.72y4
#f2= 0.8x1 + 0.29x2 - 0.41x4 + 0.12y1 + 0.2y2 + 0.77y3 + 0.21y4
# �� ������ ���� ������ ���ھ��̴�.


# ex3-2
# ������ �ҷ�����
subject = read.csv("mvadata/favoritesujects.csv", row.names = 1)
head(subject)
# (1)
subject.fact = principal(subject, nfactors = 6, rotate = "none")
subject.fact
plot(subject.fact$values, type = "b")
# �������� ũ��� ���� �л�, ��ũ�� �÷��� ���⸦ ������ �� ������ ������ ���� �� ���̴�.
# �� ���� ���ڰ� ȹ���ϴ� ������ ���� 76%�̴�.

#(2)
subject.fact = principal(subject, nfactors = 2, rotate = "none", scores = T)
subject.varimax = principal(subject, nfactors = 2, rotate = "varimax", scores = T)
subject.promax = principal(subject, nfactors = 2, rotate = "promax", scores = T)

subject.fact$loadings
# f1(BIO, GEO, CHEM, ALG, CALC, STAT)
# f2(-BIO, -GEO, -CHEM, ALG, CALC, STAT)
# f1�� �������� 2.787, f2�� 1.778 ���� �л��� ������ 0.761�̴�.
subject.varimax$loadings
# f1(BIO, GEO, CHEM)
# f2(ALG, CALC, STAT)
# f1�� �������� 2.406, f2�� 2.158 ���� �л��� ������ 0.761�̴�.
subject.promax$loadings
# f1(BIO, GEO, CHEM)
# f2(ALG, CALC, STAT)
# f1�� �������� 2.437, f2�� 2.146 ���� �л��� ������ 0.764�̴�.
# ���������� ������ �� �� ���� ���� �������� ���� �л� ������ ������ �ʴ´�.

# (3)
subject.varimax$loadings
# f1(BIO, GEO, CHEM) ����, ����, ȭ������ ���� Science�� ��Ÿ�� �� �ִ�.
# f2(ALG, CALC, STAT) ���, ������, ���� ���� Mathematics�� ��Ÿ�� �� �ִ�.

# (4)
subject.varimax
# f1(BIO, GEO, CHEM) ����, ����, ȭ������ ���� Science�� ��Ÿ�� �� �ִ�.
# f2(ALG, CALC, STAT) ���, ������, ���� ���� Mathematics�� ��Ÿ�� �� �ִ�.
# ���뼺�� stat��谡 0.52�� ���� ��������.
# �� ��Ÿ�������� �������� 76%�̴�.

subject.varimax$scores
# f1 = 0.9BIO + 0.86GEO + 0.9CHEM + 0.17STAT
# f2 = 0.122BIO + 0.145GEO + 0.889ALG + 0.914CALC + 0.702STAT

biplot(subject.varimax)
# ���� ���񱺰� ���� ���� ���� ������ ������ ���� �ٸ���.


# ex3.3
#������ �ҷ�����
beer = read.csv("mvadata/beer(2).csv")
beer

#(1)
beer.fact0 = principal(beer, nfactors = 4, rotate = "none")
beer.fact0
plot(beer.fact0$values, type = "b")
# �������� ũ�Ⱑ 1 �̻��� ��, ���� �л�, ��ũ���÷��� ���⸦ ������ �� ������ ������ ���� 2���̴�.
# ���� �� ���� �� �������� 98%�̴�.

#(2)
beer.fact = principal(beer, nfactors = 2, rotate = "none", scores = T)
beer.fact$loadings
# ���� ���� ����� ���� ����.
# f1(CALORIES, ALCOHOL)
# f2(-SODIUM, COST)

beer.varimax = principal(beer, nfactors = 2, rotate = "varimax", scores = T)
beer.varimax$loadings
# ���� ȸ�� varimax�� ���� ���� ����� ���� ����. ���� �������� ����.
# f1(CALORIES, ALCOHOL)
# f2(-SODIUM, COST)

beer.oblimin = principal(beer, nfactors = 2, rotate = "oblimin", scores = T)
beer.oblimin$loadings
# �簢 ȸ�� oblimin�� ���� ���� ����� ���� ����. ���� �������� ���� ������ ���� ������ ������ ȸ���ߴ�.
# f1(CALORIES, ALCOHOL)
# f2(-SODIUM, COST)

# (3)
beer.varimax$loadings
# f1(CALORIES, ALCOHOL) 12�½���� ������ �з����� ������ ������ ���ϰ� ���� ������ ���� �� ������ �̸��� ������ performance ������ �� �� ���� �� ����.
# f2(-SODIUM, COST) ��Ʈ�� �Է��� ������ ���ϰ� ���� ������ ����� ���ϰ� ����. ������ low sodium������ ������ �� �ִ�.

# (4)
beer.varimax
# f1(CALORIES, ALCOHOL) 12�½���� ������ �з����� ������ ������ ���ϰ� ���� ������ ���� �� ������ �̸��� ������ performance ������ �� �� ���� �� ����.
# f2(-SODIUM, COST) ��Ʈ�� �Է��� ������ ���ϰ� ���� ������ ����� ���ϰ� ����. ������ low sodium������ ������ �� �ִ�.
# ���뼺�� ��� ����.
# �� ��Ÿ�������� �������� 92%�̴�.

beer.varimax$scores
# f1 = 0.98cal + 0.43sodium + 0.96alcohol + 0.38cost
# f2 = -0.84sodium + 0.863cost

biplot(beer.varimax)
# ���ڿ� ������ ������ ������ ������ ����ϰ� ���ݰ� ��Ʈ���� ���� �� �ݴ��̴�.