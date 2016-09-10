# Wine classification

wine <- read.delim("Datasets/Wine.txt")

head(wine)
tail(wine)

# wine <- t(apply(wine, 1, standardize_vector_z))

set.seed(1234)

samp <- sample(nrow(wine), nrow(wine) / 3)
train <- wine[samp, 1:13]
test <- wine[-samp, 1:13]
cl <- wine[samp, 14]

library(class)

a <- vector()
for (i in 1:15)
{
  zzz <- knn(train, test, cl, k = i)
  a[i] <- sum(zzz != wine[-samp, 14])
}
a
# [1] 37 40 37 33 37 37 36 34 34 33 34 35 32 32 30

zzz <- knn(train, test, cl, k = 4)
table(zzz, wine[-samp, 14])
# zzz  0  1  2
#   0 37  6  5
#   1  2 14 10
#   2  1 12 32

standardize_vector_z <- function(num) {
  r <- (num - mean(num)) / sd(num)
  return(r)
}