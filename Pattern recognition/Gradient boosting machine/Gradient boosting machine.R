# Gradient boosting machine
# Пример обучения gbm для распознавания вин

library(gbm)

wine <- read.table("Datasets/Wine.txt", header = T, sep = "", dec = ".")

names(wine) <- c("Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium",
                "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins",
                "Color_intensity", "Hue", "OD280_OD315_of_diluted_wines", "Proline", "Wine_type")

dim(wine)
# [1] 178  14

set.seed(1234)

gbm.res <- gbm(Wine_type ~ . ,
               data = wine,
               distribution = "gaussian",
               n.trees = 500,
               shrinkage = 0.05,
               interaction.depth = 5,
               bag.fraction = 0.66,
               n.minobsinnode = 10,
               cv.folds = 0,
               keep.data = T,
               verbose = F)

wine.predict <- predict(gbm.res, newdata = wine[, 1:13], n.trees = 500)

summary(wine.predict)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -0.04057  0.01617  1.01200  1.06800  1.99400  2.04300

wine.predict[wine.predict <= .5] <- 0
wine.predict[(wine.predict > 0.5) & (wine.predict <= 1.5)] <- 1
wine.predict[wine.predict > 1.5] <- 2

summary(wine.predict)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.000   0.000   1.000   1.067   2.000   2.000

table(wine$Wine_type, wine.predict)
# wine.predict
#    0  1  2
# 0 59  0  0
# 1  0 48  0
# 2  0  0 71