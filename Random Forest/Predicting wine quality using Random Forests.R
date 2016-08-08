# Predicting wine quality using Random Forests
# https://www.r-bloggers.com/predicting-wine-quality-using-random-forests/

library(randomForest)

url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'
wine <- read.table(url, sep = ";", dec = ".", header = T)
head(wine)

barplot(table(wine$quality))

wine$taste <- ifelse(wine$quality < 6, 'bad', 'good')
wine$taste[wine$quality == 6] <- 'normal'
wine$taste <- as.factor(wine$taste)

# Let’s look at the distribution
table(wine$taste)
# bad    good normal 
# 1640   1060   2198

set.seed(123)
samp <- sample(nrow(wine), 0.6 * nrow(wine))
train <- wine[samp, ]
test <- wine[-samp, ]

# We can use ntree and mtry to specify the total number of trees to build (default = 500),
# and the number of predictors to randomly sample at each split respectively.
model <- randomForest(taste ~ . - quality, data = train)

model
# Call:
#   randomForest(formula = taste ~ . - quality, data = train) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3
# 
# OOB estimate of  error rate: 29.82%
# Confusion matrix:
#        bad good normal class.error
# bad    671   18    284   0.3103803
# good    17  402    230   0.3805855
# normal 221  106    989   0.2484802

pred <- predict(model, newdata = test)
table(pred, test$taste)
# pred     bad good normal
#   bad    481   12    128
#   good    13  247     81
#   normal 173  152    673

# We can test the accuracy as follows:
(481 + 247 + 673) / nrow(test)
# 0.7147959