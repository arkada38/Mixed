# Hierarchical Clustering

# Exploring the data

library(datasets)
head(iris)
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

# Clustering

irisCluster <- hclust(dist(iris[, 3:4]), "ward.D")
irisCluster
# Call:
# hclust(d = dist(iris[, 3:4]), method = "ward.D")
#
# Cluster method   : ward.D
# Distance         : euclidean
# Number of objects: 150

# Construction of the dendrogram
plot(irisCluster)
rect.hclust(irisCluster, k = 3, border = "red")

# Divide into 3 clusters
# groups vector contains the cluster number 
groups <- cutree(irisCluster, k = 3)
groups

table(groups, iris$Species)
# groups setosa versicolor virginica
#      1     50          0         0
#      2      0         45         1
#      3      0          5        49

groups <- as.factor(groups)
ggplot(iris, aes(Petal.Length, Petal.Width, color = groups)) + geom_point()

library(gridExtra)
plot1 <- ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
plot2 <- ggplot(iris, aes(Petal.Length, Petal.Width, color = groups)) + geom_point()
grid.arrange(plot1, plot2, nrow = 1, ncol = 2)