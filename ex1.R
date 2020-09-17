#1.1
# ������ ����
convinience.v <- c(70.5, 64.8,67.1,61.1,63.4,72.3,64.2,68.4,66.1,63.5,69,63.2,64.1)
accuracy.v <- c(59.4, 70.3,79.6,65,66.5,69.1,72,67.5,66.5,65.7,74.3,65.5,64.8)
kindness.v <- c(63.7, 68.6, 78.5,65.6,67.9,74.2,71.4,67.3,67.3,64.3,80.5,68.3,67.8)
efficiency.v <- c(54.3, 55.2, 62.4,54.4,65,60,56.9,51.3,50.7,53.9,63.6,49.8,59.7)
pleasant.v <- c(66.9, 68, 79.8, 64.5, 59.7,70.1,72.8,71.3,63.4,61.7,75.7,64.6,65.7)
automatic.v <- c(62.6, 64.1, 62.4,63.9,62,68.2,57.8,65.8,63.3,62.7,55.9,59.1,61.8)
rows = c("kookmin", "enterpr", "boram", "commerce", "seoul", "shinhan", "city",
         "exchange", "first", "chohung", "hana", "hanil", "house")
cols = c("convinience", "accuracy", "kindness", "efficiency", "pleasant", "automatic")
bank <- data.frame(convinience.v, accuracy.v, kindness.v, efficiency.v, pleasant.v, automatic.v, row.names = rows)
colnames(bank) <- cols

#(1)
attach(bank)
hist(convinience)
# ���������� ������ �þ��� ���� ���� �ֵ��� ����� ������ �����ȴ�.
hist(accuracy)
# ��¦ ��Ī���̰� ����� �ھ� �ִ�. �ֵ��� 0�� ������ ÷���� ���� ������ ����ȴ�.
hist(kindness)
# ���������� ������ �ְ� ��պκ��� ���� ������� �ֵ��� ����̰� ÷���� ���� ������ ����ȴ�.
hist(efficiency)
# ���������� ������ ������ ��ü������ �յ��� ����̴�.
hist(pleasant)
# ���������� ������ �ִ�
hist(automatic)
# �������� ������ �ְ� ÷���� ������ �����ʿ� �̻��� �����Ѵ�.

#(2)
plot(bank)
cor(bank)
# accuracy(�żӼ�)�� kindness(ģ��)�� ���� ���� ������踦 ������.
# accuracy(�żӼ�)�� efficiency(�ɷ�)�� ���� ���� �������
# accuracy(�żӼ�)�� pleasant(����)�� ���� ���� ������谡 �ִ�.
# kindness(ģ��)�� efficiency(�ɷ�)�� ���� ������踦 ���δ�.
# kindness(ģ��)�� pleasant(����)�� ���� ���� ������踦 ���δ�.
# kindness(ģ��)�� convinience(������)�� ���� ���� ������踦 ���δ�.

#(3)
library(aplpack)
stars(bank)
faces(bank)
# boram�� shinhan�� ��ü������ ����ϸ� hanil�� ���� �򰡸� �ް� �ִ�.

#----------------------------------------------------------------------------
# 1.2
# ������ ����
charity <- matrix(c(5.4, 3.1, 3.5, 5.7, 8.6, 25, 20.4, 26, 22, 36.3, 34.1, 28, 14.4, 11.4,  4.5),
                  ncol = 3, byrow = T)
rownames(charity) <- paste(rep("T", 5), 1:5, sep = "")
colnames(charity) <- paste(rep("C", 3), 1:3, sep = "")
charity

#(1)
barplot(charity)
#(2)
pie(charity[, "C1"])

#-------------------------------------------------------------

# 1.3
#(1)
tdist <- rt(100, 5)

#(2)
hist(tdist)
# �������� ������ ������ �̻��� �����Ѵ�. 

#(3)
boxplot(tdist)
# �̻��� �����Ѵ�.

#(4)
stem(tdist)
# 0�ٱ⿡ ���� ���� ���� �����ϸ� -4�� �̻��� ���Ѵ�.

#---------------------------------------------------------
#1.4
head(longley)
plot(longley)
stars(longley)
faces(longley)
# Unemployed�� Armed.Forces�� ������ ������ ������ ����� ���� ����ϴ� ������ ���δ�.
# �����, �α�, �����ѻ��� ���� ����ϴ� ���̹Ƿ� ������ ������ �������� �� �� �ִ�.

#--------------------------------------------------------------------------
#1.5
library(HSAUR2)
attach(USairpollution)
#(1)
plot(USairpollution)
stars(USairpollution)
faces(USairpollution)
# �ʶ��Ǿƿ� �˹ٴϰ� ������ ���� ���ϴ�.

#(2)
library(MVA)
bvbox(USairpollution[,c("temp", "wind")], xlab = "temp", ylab = "wind")
# wind 6�� 9 temp 70�� �Ѵ� �̻��� �����Ѵ�.

#(3)
plot(manu~popul)
symbols(manu~popul, circles = SO2, add = T)
# manu�� popul�� ���� ������ ���踦 ���̸� �ſ� ū �뵵�ð� �����Ѵ�. 
# popul�� manu�� �۾Ƶ� SO2�� ���� ���õ� �����Ѵ�.