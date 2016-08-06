# K Means cluster analysis (step by step)

# beverage <- read.table("Data_beverage.csv", header = T, sep = ";")
# head(beverage)
# summary(beverage)
# 
# beverage$numb.obs <- NULL

set.seed(1234)

# quantity of observations in each cluster
n.obs <- 900
# standart deviation
sd.1 <- 0.06

# Generation of clusters
x1 <- rnorm(n.obs, mean = .20, sd = sd.1)
y1 <- rnorm(n.obs, mean = .38, sd = sd.1)

x2 <- rnorm(n.obs, mean = .49, sd = sd.1)
y2 <- rnorm(n.obs, mean = .25, sd = sd.1)

x3 <- rnorm(n.obs, mean = .62, sd = sd.1)
y3 <- rnorm(n.obs, mean = .42, sd = sd.1)

x4 <- rnorm(n.obs, mean = .42, sd = sd.1)
y4 <- rnorm(n.obs, mean = .78, sd = sd.1)

x5 <- rnorm(n.obs, mean = .85, sd = sd.1)
y5 <- rnorm(n.obs, mean = .75, sd = sd.1)

# Creation one matrix
x.0 <- c(x1, x2, x3, x4, x5)
y.0 <- c(y1, y2, y3, y4, y5)
data.0 <- cbind(x.0, y.0)

colnames(data.0) <- c("x", "y")

# Centers of klusters
x.start <- c(.50, .41, .43, .62, .38)
y.start <- c(.20, .22, .32, .36, .71)
centers.0 <- cbind(x.start, y.start)

# Step by step clustering
clus.1  <- kmeans(data.0, centers = centers.0,       iter.max = 1, algorithm = "Lloyd")
clus.2  <- kmeans(data.0, centers = clus.1$centers,  iter.max = 1, algorithm = "Lloyd")
clus.3  <- kmeans(data.0, centers = clus.2$centers,  iter.max = 1, algorithm = "Lloyd")
clus.4  <- kmeans(data.0, centers = clus.3$centers,  iter.max = 1, algorithm = "Lloyd")
clus.5  <- kmeans(data.0, centers = clus.4$centers,  iter.max = 1, algorithm = "Lloyd")

clus.6  <- kmeans(data.0, centers = clus.5$centers,  iter.max = 1, algorithm = "Lloyd")
clus.7  <- kmeans(data.0, centers = clus.6$centers,  iter.max = 1, algorithm = "Lloyd")
clus.8  <- kmeans(data.0, centers = clus.7$centers,  iter.max = 1, algorithm = "Lloyd")
clus.9  <- kmeans(data.0, centers = clus.8$centers,  iter.max = 1, algorithm = "Lloyd")
clus.10 <- kmeans(data.0, centers = clus.9$centers,  iter.max = 1, algorithm = "Lloyd")

clus.11 <- kmeans(data.0, centers = clus.10$centers, iter.max = 1, algorithm = "Lloyd")
clus.12 <- kmeans(data.0, centers = clus.11$centers, iter.max = 1, algorithm = "Lloyd")
clus.13 <- kmeans(data.0, centers = clus.12$centers, iter.max = 1, algorithm = "Lloyd")
clus.14 <- kmeans(data.0, centers = clus.13$centers, iter.max = 1, algorithm = "Lloyd")
clus.15 <- kmeans(data.0, centers = clus.14$centers, iter.max = 1, algorithm = "Lloyd")



# Size of points in plot
cex.1 <- .2
# Colors of clusters
col.1 <- c("green", "blue", "cyan", "purple", "darkgoldenrod")



plot(data.0, col = "blue", pch = 19, main = "Iteration 0", cex = cex.1)
points(x.start, y.start, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)

plot(data.0, col = col.1[clus.1$cluster], pch = 19, main = "Iteration 1", cex = cex.1)
points(x.start, y.start, col = "black", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)
points(clus.1$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)
arrows(x.start, y.start, x1 = clus.1$centers[,1], y1 = clus.1$centers[,2],
       col = "red", lwd = 3, angle = 15, length = .2)

plot(data.0, col = col.1[clus.2$cluster], pch = 19, main = "Iteration 2", cex = cex.1)
points(clus.1$centers, col = "black", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)
points(clus.2$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)
arrows(clus.1$centers[,1], clus.1$centers[,2], x1 = clus.2$centers[,1], y1 = clus.2$centers[,2],
       col = "red", lwd = 3, angle = 15, length = .2)

plot(data.0, col = col.1[clus.3$cluster], pch = 19, main = "Iteration 3", cex = cex.1)
points(clus.2$centers, col = "black", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)
points(clus.3$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)
arrows(clus.2$centers[,1], clus.2$centers[,2], x1 = clus.3$centers[,1], y1 = clus.3$centers[,2],
       col = "red", lwd = 3, angle = 15, length = .2)

plot(data.0, col = col.1[clus.4$cluster], pch = 19, main = "Iteration 4", cex = cex.1)
points(clus.3$centers, col = "black", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)
points(clus.4$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)
arrows(clus.3$centers[,1], clus.3$centers[,2], x1 = clus.4$centers[,1], y1 = clus.4$centers[,2],
       col = "red", lwd = 3, angle = 15, length = .2)

plot(data.0, col = col.1[clus.5$cluster], pch = 19, main = "Iteration 5", cex = cex.1)
points(clus.4$centers, col = "black", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)
points(clus.5$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)
arrows(clus.4$centers[,1], clus.4$centers[,2], x1 = clus.5$centers[,1], y1 = clus.5$centers[,2],
       col = "red", lwd = 3, angle = 15, length = .2)

plot(data.0, col = col.1[clus.6$cluster], pch = 19, main = "Iteration 6", cex = cex.1)
points(clus.6$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)

plot(data.0, col = col.1[clus.7$cluster], pch = 19, main = "Iteration 7", cex = cex.1)
points(clus.7$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)

plot(data.0, col = col.1[clus.8$cluster], pch = 19, main = "Iteration 8", cex = cex.1)
points(clus.8$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)

plot(data.0, col = col.1[clus.9$cluster], pch = 19, main = "Iteration 9", cex = cex.1)
points(clus.9$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)

plot(data.0, col = col.1[clus.10$cluster], pch = 19, main = "Iteration 10", cex = cex.1)
points(clus.10$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)

plot(data.0, col = col.1[clus.11$cluster], pch = 19, main = "Iteration 11", cex = cex.1)
points(clus.11$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)

plot(data.0, col = col.1[clus.12$cluster], pch = 19, main = "Iteration 12", cex = cex.1)
points(clus.12$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)

plot(data.0, col = col.1[clus.13$cluster], pch = 19, main = "Iteration 13", cex = cex.1)
points(clus.13$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)

plot(data.0, col = col.1[clus.14$cluster], pch = 19, main = "Iteration 14", cex = cex.1)
points(clus.14$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)

plot(data.0, col = col.1[clus.15$cluster], pch = 19, main = "Iteration 15", cex = cex.1)
points(clus.15$centers, col = "red", xlim = c(0, 1), ylim = c(0, 1.1), cex = cex.1 * 9, pch = 19)
