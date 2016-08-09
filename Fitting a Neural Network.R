# Fitting a Neural Network
# http://datascienceplus.com/fitting-neural-network-in-r/

set.seed(500)
library(MASS)

# The Boston dataset is a collection of data about housing values in the suburbs of Boston.
# Our goal is to predict the median value of owner-occupied homes (medv)
# using all the other continuous variables available.
data <- Boston

# Checking that no datapoint is missing
apply(data, 2, function(x) sum( is.na(x) ))
# crim      zn   indus    chas     nox      rm     age     dis     rad     tax ptratio   black   lstat    medv 
#    0       0       0       0       0       0       0       0       0       0       0       0       0       0

index <- sample(1:nrow(data), round(.75 * nrow(data)))
train <- data[index,]
test <- data[-index,]

lm.fit <- glm(medv~., data = train)
summary(lm.fit)
# Call:
# glm(formula = medv ~ ., data = train)
# 
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -16.6267   -2.9502   -0.4378    1.8204   25.2156  
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  34.877261   6.030939   5.783 1.57e-08 ***
# crim         -0.108726   0.037171  -2.925 0.003659 ** 
# zn            0.060180   0.015458   3.893 0.000118 ***
# indus         0.046443   0.071005   0.654 0.513477    
# chas          3.351648   1.024079   3.273 0.001166 ** 
# nox         -14.874195   4.493834  -3.310 0.001026 ** 
# rm            3.657815   0.499908   7.317 1.62e-12 ***
# age           0.015714   0.016062   0.978 0.328575    
# dis          -1.338569   0.233303  -5.737 2.02e-08 ***
# rad           0.395489   0.084781   4.665 4.34e-06 ***
# tax          -0.016897   0.004738  -3.566 0.000410 ***
# ptratio      -0.927366   0.151624  -6.116 2.46e-09 ***
# black         0.009079   0.003210   2.828 0.004942 ** 
# lstat        -0.581928   0.062568  -9.301  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 23.43144)
# 
#     Null deviance: 32352.7  on 379  degrees of freedom
# Residual deviance:  8575.9  on 366  degrees of freedom
# AIC: 2292.7
# 
# Number of Fisher Scoring iterations: 2

pr.lm <- predict(lm.fit, test)
# We are going to use the mean squared error (MSE) as a measure
# of how much our predictions are far away from the real data.
MSE.lm <- sum((pr.lm - test$medv) ^ 2) / nrow(test)
# [1] 21.3585



# Preparing to fit the neural network

# Normalization
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

# Parameters
# We are going to use 2 hidden layers with this configuration: 13:5:3:1.
# The input layer has 13 inputs, the two hidden layers have 5 and 3 neurons and the output layer has,
# of course, a single output since we are doing regression.

library(neuralnet)
n <- names(train_)
f <- as.formula( paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")) )
nn <- neuralnet(f, data = train_, hidden = c(5,3), linear.output = T)
plot(nn)

# Predicting medv using the neural network
pr.nn <- compute(nn, test_[,1:13])

pr.nn_ <- pr.nn$net.result * (max(data$medv) - min(data$medv)) + min(data$medv)
test.r <-       test_$medv * (max(data$medv) - min(data$medv)) + min(data$medv)

MSE.nn <- sum((test.r - pr.nn_) ^ 2) / nrow(test_)
# [1] 13.36667375

print(paste(MSE.lm,MSE.nn))
# [1] "21.358503721077 13.3666737470421"



# A perfect alignment with the line would indicate a MSE of 0 and thus an ideal perfect prediction
par(mfrow = c(1,2))

plot(test$medv, pr.nn_, col = 'red', main = 'Real vs predicted NN', pch = 18, cex = .7)
abline(0, 1, lwd = 2)
legend('bottomright', legend = 'NN', pch = 18, col = 'red', bty = 'n')

plot(test$medv, pr.lm, col = 'blue', main = 'Real vs predicted lm', pch = 18, cex = .7)
abline(0, 1, lwd = 2)
legend('bottomright', legend = 'LM', pch = 18, col = 'blue', bty = 'n', cex = .95)

par(mfrow = c(1,1))

plot(test$medv, pr.nn_, col = 'red', main = 'Real vs predicted NN', pch = 18, cex = .7)
points(test$medv, pr.lm, col = 'blue', pch = 18, cex = .7)
abline(0, 1, lwd = 2)
legend('bottomright', legend = c('NN','LM'), pch = 18, col = c('red', 'blue'))



# A (fast) cross validation

library(boot)
set.seed(200)

# Here is the 10 fold cross validated MSE for the linear model
lm.fit <- glm(medv~., data = data)
cv.glm(data, lm.fit, K = 10)$delta[1]

# 90% train set and 10% test set in a random way for 10 times
set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample( 1:nrow(data), round(.9 * nrow(data)) )
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f, data = train.cv, hidden = c(5,2), linear.output = T)
  
  pr.nn <- compute(nn, test.cv[,1:13])
  pr.nn <- pr.nn$net.result * (max(data$medv) - min(data$medv)) + min(data$medv)
  
  test.cv.r <- (test.cv$medv) * (max(data$medv) - min(data$medv)) + min(data$medv)
  
  cv.error[i] <- sum((test.cv.r - pr.nn) ^ 2) / nrow(test.cv)
  
  pbar$step()
}

mean(cv.error)
# [1] 10.32697995

cv.error
# [1] 17.640652805  6.310575067 15.769518577  5.730130820 10.520947119  6.121160840  6.389967211  8.004786424
# [9] 17.369282494  9.412778105

boxplot(cv.error, xlab='MSE CV', col = 'cyan',
        border = 'blue', names = 'CV error (MSE)',
        main = 'CV error (MSE) for NN', horizontal = T)

hist(cv.error)