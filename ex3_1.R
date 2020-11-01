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
#x1= 0.26f1 + 0.80f2 + e1
#x2= 0.67f1 + 0.29f2 + e2
#x3= 0.75f1 + 0.01f2 + e3
#x4= 0.53f1 - 0.41f2 + e4
#y1= 0.71f1 + 0.12f2 + e5
#y2= 0.84f1 + 0.20f2 + e6
#y3= 0.11f1 + 0.77f2 + e7
#y4= 0.72f1 + 0.21f2 + e8
# �� ������ ���� ������ ���ھ��̴�.
