# K Means cluster analysis

beverage <- read.table("Datasets/beverage.csv", header = T, sep = ";")
head(beverage)
summary(beverage)

beverage$numb.obs <- NULL

set.seed(1234)


# Set 3 clusters (like in hierarchical)
summ.3 = kmeans(beverage, 3, iter.max = 100, nstart = 500)


names(summ.3)
# [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
# [6] "betweenss"    "size"         "iter"         "ifault"  


# In which cluster every object?
summ.3$cluster
# [1] 3 2 2 1 2 3 1 3 3 2 3 1 3 3 1 3 3 2 2 2 2 2 1 2 1 1 1 3 3 1 2 1 2 1

options(digits = 2)
# Centers of clusters
summ.3$centers
#   COKE D_COKE D_PEPSI D_7UP PEPSI SPRITE   TAB SEVENUP
# 1 0.00   1.00   0.545 0.545  0.00      0 0.909    0.00
# 2 1.00   0.25   0.083 0.000  0.92      0 0.000    0.33
# 3 0.73   0.27   0.091 0.091  0.45      1 0.091    0.45

# Transpose
# Mean for every var in each cluster
t(summ.3$centers)
#            1     2     3
# COKE    0.00 1.000 0.727
# D_COKE  1.00 0.250 0.273
# D_PEPSI 0.55 0.083 0.091
# D_7UP   0.55 0.000 0.091
# PEPSI   0.00 0.917 0.455
# SPRITE  0.00 0.000 1.000
# TAB     0.91 0.000 0.091
# SEVENUP 0.00 0.333 0.455

options(digits = 7)

# Сумма квадратов расстояний от объектов кластера до центра кластера
summ.3$withinss
# [1]  6.363636  6.750000 12.545455

# sum(summ.3$withinss)
summ.3$tot.withinss
# [1] 25.65909

# (nrow(beverage) - 1) * sum( apply(beverage, 2, var) )
summ.3$totss
# [1] 58.38235

# The sizes of clusters
summ.3$size
# [1] 11 12 11




# Now determing a quantity of clusters between 2 and 15
# The best of 100 iterations with 500 attempts goes to wss[i]
wss <- vector()
for (i in 2:15)
  wss[i] <- kmeans(beverage, centers = i, iter.max = 100, nstart = 500)$tot.withinss

# 3 clusters (angle more acute)
# After 3 improved of clustering is insignificantly
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")



# Comparison 3 clusters with 4

summ.4 = kmeans(beverage, 4, iter.max = 100, nstart = 500)
table(summ.3$cluster, summ.4$cluster)
#    1  2  3  4
# 1  0  0 11  0
# 2  0 11  0  1
# 3  6  0  0  5



# Multidimentional scaling
beverage.dist <- dist(beverage)
beverage.mds <- cmdscale(beverage.dist)
plot(beverage.mds, col = summ.3$cluster, xlab = "Index", ylab = "Y")
plot(beverage.mds, col = summ.4$cluster, xlab = "Index", ylab = "Y")





library(NbClust)
# Ball and Hall 1965
NbClust(beverage, method = "kmeans", index = "ball")$Best.nc
# Number_clusters     Value_Index 
#           3.000           7.735

