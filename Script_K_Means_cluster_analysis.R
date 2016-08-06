# K Means cluster analysis

beverage <- read.table("Data_beverage.csv", header = T, sep = ";")
head(beverage)
summary(beverage)

beverage$numb.obs <- NULL

set.seed(1234)


# Set 3 clusters (like in hierarchical)
summ = kmeans(beverage, 3, iter.max = 100)


names(summ)
# [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
# [6] "betweenss"    "size"         "iter"         "ifault"  


# In which cluster every object?
summ$cluster
# [1] 3 2 2 1 2 2 1 3 3 2 2 1 3 3 1 3 3 2 3 2 2 2 1 2 1 1 1 3 3 1 2 1 2 1

options(digits = 2)
# Centers of clusters
summ$centers
#   COKE D_COKE D_PEPSI D_7UP PEPSI SPRITE  TAB SEVENUP
# 1  0.0   1.00   0.545  0.55   0.0   0.00 0.91    0.00
# 2  1.0   0.23   0.077  0.00   1.0   0.15 0.00    0.23
# 3  0.7   0.30   0.100  0.10   0.3   0.90 0.10    0.60

# Transpose
# Mean for every var in each cluster
t(summ$centers)
#            1     2   3
# COKE    0.00 1.000 0.7
# D_COKE  1.00 0.231 0.3
# D_PEPSI 0.55 0.077 0.1
# D_7UP   0.55 0.000 0.1
# PEPSI   0.00 1.000 0.3
# SPRITE  0.00 0.154 0.9
# TAB     0.91 0.000 0.1
# SEVENUP 0.00 0.231 0.6

options(digits = 7)

# Сумма квадратов расстояний от объектов кластера до центра кластера
summ$withinss
# [1]  6.363636  7.230769 12.300000

# sum(summ$withinss)
summ$tot.withinss
# [1] 25.89441

# (nrow(beverage) - 1) * sum( apply(beverage, 2, var) )
summ$totss
# [1] 58.38235

# The sizes of clusters
summ$size
# [1] 11 13 10




# Now determing a quantity of clusters between 2 and 15

for (i in 2:15)
  wss[i] <- kmeans(beverage, centers = i)$tot.withinss

# 3 clusters (angle more acute)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")



# Comparison 3 clusters with 4

summ.4 = kmeans(beverage, 4, iter.max = 100)
table(summ$cluster, summ.4$cluster)
#    1  2  3  4
# 1  0 11  0  0
# 2  3  0  0 10
# 3  3  0  6  1




# Multidimentional scaling

beverage.dist <- dist(beverage)
beverage.mds <- cmdscale(beverage.dist)
clust.beverage <- hclust(beverage.dist, "ward.D")
groups <- cutree(clust.beverage, k=3) 

plot(beverage.mds, col = groups, xlab = "Index", ylab = "Y")





library(NbClust)
# All indices with Gap, Gamma, Gplus and Tau included
Best <- NbClust(beverage, distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward.D", index = "alllong")