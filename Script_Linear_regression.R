# Linear regression
# http://www.algorithmist.ru/2011/04/linear-regression-with-examples-in-r.html#more

# Reading data from a file
data <- read.table('Data_diamond.dat')
names(data) <- c('Weight', 'Price')

summary(data)

Price <- data$Price
Weight <- data$Weight

# Building a linear regression
fit <- lm(Price ~ Weight)
summary(fit)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -259.63      17.32  -14.99   <2e-16 ***
# data$Weight  3721.02      81.79   45.50   <2e-16 ***

# Price = -259.63 + 3721.02 * Weight
# The model describes the data on 97.83%


plot(Price ~ Weight,
     main = "Price ~ Weight",
     sub = "Price = -259.63 + 3721.02 * Weight",
     xlab = "The weight of a diamond (carat)", 
     ylab = "The price of the ring in Singapore dollars")
abline(fit, col = 2)


# Predict
newdata <- data.frame(Weight = c(.15, .18, .23, .33))
p <- predict(fit, newdata)
#        1        2        3        4 
# 298.5278 410.1586 596.2098 968.3123
newdata$Price <- p
points(newdata$Weight, y = newdata$Price, type = "p", col = 4, pch = 19)



library(party)

data <- readingSkills
data$result <- as.numeric(readingSkills$nativeSpeaker) - 1

# Print some records from data
print(head(data))

# Строим линейную регрессию
fit <- lm(data$result ~ data$age + data$shoeSize + data$score )

summary(fit)

# Result = -0.816931 -0.644451 * age -0.006149 * shoeSize + 0.162203 * score
# The model describes the data on 97.07%