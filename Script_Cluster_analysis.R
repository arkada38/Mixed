# Cluster analysis

beverage.01 <- read.table("Data_beverage.csv", header = T, sep = ";")
head(beverage.01)
summary(beverage.01)

beverage.01$numb.obs <- NULL

# Проводим кластерный анализ,
# результаты записываем в список clust.beverage

clust.beverage <- hclust(dist(beverage.01), "ward.D")

#  Смотрим краткую сводку результатов анализа
clust.beverage
# Cluster method   : ward.D 
# Distance         : euclidean 
# Number of objects: 34

# Построение дендрограммы
plot(clust.beverage)
rect.hclust(clust.beverage, k = 3, border = "red")

# Разделим пользователей на 3 кластера
# Вектор groups содержит номер кластера,
# в который попал классифицируемый объект 
groups <- cutree(clust.beverage, k = 3) 
groups

# в 1-ом кластере
colMeans(beverage.01[groups==1,]) * 100
# во 2-ом кластере
colMeans(beverage.01[groups==2,]) * 100
# в 3-ем кластере
colMeans(beverage.01[groups==3,]) * 100
