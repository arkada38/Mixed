# Gradient boosting machine
# Пример  обучения gbm для распознавания вин

library(gbm)

zzz <- read.table("Datasets/Wine.txt", header = T, sep = "", dec = ".")
names(zzz) <- c("Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium",
                "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins",
                "Color_intensity", "Hue", "OD280_OD315_of_diluted_wines", "Proline", "Wine_type")

dim(zzz)
x <- zzz[, 1:13]
y <- zzz[, 14]

y.1 <- as.factor(y)
zzz.1 <- data.frame(x, y)
names(zzz.1) <- c("Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium",
                  "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins",
                  "Color_intensity", "Hue", "OD280_OD315_of_diluted_wines", "Proline", "Wine_type")

set.seed(3217)


ntree.1 <- 500
nodesize.1 <- 10
keep.forest.1 <- TRUE

gbm.res <- gbm(Wine_type ~ . , data = zzz.1, distribution = "gaussian",
               n.trees = ntree.1,
               shrinkage = 0.05,
               interaction.depth = 5,
               bag.fraction = 0.66,
               n.minobsinnode = nodesize.1,
               cv.folds = 0,
               keep.data = T,
               verbose = F) # don’t print out progress

zzz.predict <- predict(gbm.res, newdata = x, n.trees = ntree.1)
zzz.predict[1:11]

zzz.pr.2 <- rep(0, nrow(zzz))
zzz.pr.2[(zzz.predict > 0.5) & (zzz.predict < 1.5)] <- 1
zzz.pr.2[zzz.predict > 1.5] <- 2

table(zzz.1$Wine_type, zzz.pr.2)
# zzz.pr.2
#    0  1  2
# 0 59  0  0
# 1  0 48  0
# 2  0  0 71

# Строю GBM, для факторов 2 - 11 ====
# Независимые переменные - ответы на вопросы анкеты # Зависимая переменная - фактор 
# 2-я версия данных
# Чрезмерное использование команды  Sys.time() - простейший вариант # profiling'a, при нормальной работе сервера эта команда не нужна
#  в данной версии скрипта строится 1000 деревьев
# Построенные деревья сохраняются в файлах gbm_res_1000_1_fNNN.RData # где NNN - номер фактора
# Осторожно!!!! #  Результаты для каждой модели имеют одно и то же имя  gbm.res.f1
# 0 ======================================================= # Подготовка данных
setwd("...")
#  читаем необходимые данные
#  значения факторов sid.factors.short.1 <- read.table("sid_factors_3", header=F, sep=";", stringsAsFactors = F)
#  ответы на вопросы + в последнем столбце значения первого фактора zzz <- read.table("data_factor_y2", header=F, sep=";", stringsAsFactors = F, nrows = 45000)
library(gbm)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #   Определяем, для которого фактора строим модель fact.num <- 2
#   Делаем зависимой переменной выбранный фактор (он в отдельной таблице) zzz[ , ncol(zzz)] <- sid.factors.short.1[ , fact.num + 1]
names(zzz)[length(names(zzz))] <- "y.factor"
gbm.res.f1 <- gbm(y.factor~. , data=zzz, distribution="bernoulli",  # !!!!!!!!!!!!!!!!!!   "gaussian", "laplace"
                 n.trees=1000,                 # !!!!!!!!!!!!!!!!!!   500
                 shrinkage=0.05,             # !!!!!!!!!!!!!!!!!!   0.01
                 interaction.depth=5,      #  !!!!!!!!!!!!!!!!!!    3 -7
                 bag.fraction = 0.66,       #  !!!!!!!!!!!!!!!!!!   0.5
                 n.minobsinnode = 10,    #  !!!!!!!!!!!!!!!!!!   1
                 cv.folds = 0,                  #   !!!!!!!!!!!!!!!!!!
                 keep.data=TRUE,         #   !!!!!!!!!!!!!!!!!!   но не в production
                 verbose=FALSE)           # don’t print out progress

save(gbm.res.f1, file = "gbm_res_1000_1_f2.RData")
                 rm(gbm.res.f1)
                 # ================================================ # Описание качества модели
                 # MSE sum((gbm.res.f1$fit - zzz[ , ncol(zzz)] )^2)/length(gbm.res.f1$fit)
                 # MAE == MAD sum(abs(gbm.res.f1$fit - zzz[ , ncol(zzz)] ))/length(gbm.res.f1$fit)
                 # Описательные статистики для абсолютных отклонений summary(abs(gbm.res.f1$fit - zzz[ , ncol(zzz)] ))