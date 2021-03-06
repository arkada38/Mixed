# Hierarchical cluster analysis

beverage <- read.table("Datasets/beverage.csv", header = T, sep = ";")
head(beverage)
summary(beverage)

beverage$numb.obs <- NULL

# We perform cluster analysis,
# The result is written to the list clust.beverage

clust.beverage <- hclust(dist(beverage), "ward.D")
clust.beverage
# Cluster method   : ward.D 
# Distance         : euclidean 
# Number of objects: 34

# Construction of the dendrogram
plot(clust.beverage)
rect.hclust(clust.beverage, k = 3, border = "red")

# Divide into 3 clusters
# groups vector contains the cluster number 
groups <- cutree(clust.beverage, k = 3)
groups

# in the first cluster
colMeans(beverage[groups==1,]) * 100
# in the second cluster
colMeans(beverage[groups==2,]) * 100
# in the 3rd cluster
colMeans(beverage[groups==3,]) * 100

# Multidimentional scaling
beverage.dist <- dist(beverage)
beverage.mds <- cmdscale(beverage.dist)
plot(beverage.mds, col = groups, main = "Hierarchical cluster analysis", xlab = "Index", ylab = "Y")

library(NbClust)
# Ball and Hall 1965
NbClust(beverage, method = "ward.D", index = "ball")$Best.nc
# Number_clusters     Value_Index 
#          3.0000          8.0434