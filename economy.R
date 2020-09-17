# 1
library(aplpack)
econo.data = read.csv('econo.csv', header = T, row.names = 'year')

stars(econo.data)
faces(econo.data)
# 별그림과 얼굴 그림의 크기가 커지는 것을 볼 수 있다.
# 우리 나라의 경제가 지속적으로 성장하는 것을 보여준다.

#------------------------------------------------------------------------
# 2
econo.pca = princomp(econo.data, cor = F, scores = T)
summary(econo.pca)
# PC1의 Proportion of Variance가 0.99로 매우 설명력이 있다. 
# PC1 하나로 총변이에 대한 공헌도가 80%가 넘으므로 나머지 PC를 무시할 수 있다.
# 표준화한 것이 아닌 분산 공분산 행렬 기반이기 때문에 람다의 크기로는 판단할 수 없다.
screeplot(econo.pca, type = "line", pch = 19, main = "Scree Plot")
# 스크리플롯을 보았을 때 pc1에서 2로 넘어갈 때 가파르고 그 이후 완만하므로 pc1만 포함한다.

round(econo.pca$loadings[, 1], 2)
# PC1 = 0.00x1 + 0.06x2 + 0.11x3 + 0.05x4 + 0.06x5 + 0.56x6
# + 0.52x7 + 0.63x8 + 0.00x9 + 0.00x10 + 0.02x11 + 0.07x12 + 0.01x13으로 나타낼수 있다.
econo.pca$score[, 1]
# 년도가 지날 수록 주성분의 값이 커진다. 즉 우리 경제가 전체적으로 성장한다는 것을 보인다.

#-------------------------------------------------------------------------
# 3
econo.pca = princomp(econo.data, cor = T, scores = T)
summary(econo.pca)
# PC1의 Proportion of Variance가 0.94로 매우 설명력이 있다. 
# PC1 하나로 총변이에 대한 공헌도가 80%가 넘으므로 나머지 PC를 무시할 수 있다.
# PC1에 대응하는 람다1 즉 std^2 = 3.49^2 = 12이고 나머지 PC들은 std가 소수로 제곱해도 1이 안 된다.
# 따라서 람다가 1이 되지 않는 것들을 제외하므로 PC1만 남는다.
screeplot(econo.pca, type = "line", pch = 19, main = "Scree Plot")
# 스크리플롯을 보았을 때 pc1에서 2로 넘어갈 때 가파르고 그 이후 완만하므로 pc1만 포함한다.

round(econo.pca$loadings[, 1], 2)
# PC1 = 0.28z1 + 0.28z2 + 0.28z3 + 0.28z4 + 0.28z5 + 0.28z6
# + 0.28z7 + 0.28z8 + 0.25z9 + 0.28z10 + 0.27z11 + 0.27z12 + 0.27z13으로 나타낼수 있다.
econo.pca$score[, 1]
# 년도가 지날 수록 주성분의 값이 커진다. 즉 우리 경제가 전체적으로 성장한다는 것을 보인다.
