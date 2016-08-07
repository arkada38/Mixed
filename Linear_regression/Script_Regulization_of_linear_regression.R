# http://www.algorithmist.ru/2011/11/regularization-with-examples-in-r.html

gen <- function(x){
  # данные распределены по формуле 3*x2+2*x+2
  # плюс некоторая погрешность, случайным образом распределенная
  return(3 * x^2 + 2 * x + 2 + rnorm(length(x)) * .5)
}



#Генерируем тренировочные данные
X <- runif(6)
Y <- gen(X)

#Данные для кросс-валидации
Xcv <- runif(50)
Ycv <- gen(Xcv)



#Линейная регрессия по полиномиальному набору данных
train <- data.frame(Y, X, X^2, X^3, X^4, X^5)
colnames(train) <- c('Y', 'X', 'X2', 'X3', 'X4', 'X5')
simple <- lm(Y ~ ., train)
error <- sum((predict(simple, train) - Y)^2) / length(Y)
cat("Train error: ",error,"\n")

cv <- data.frame(Xcv, Xcv^2, Xcv^3, Xcv^4, Xcv^5)
colnames(cv) <- c('X', 'X2', 'X3', 'X4', 'X5')
error <- sum((predict(simple, cv) - Ycv)^2) / length(Ycv)
cat("Cross-validation error: ",error,"\n")



#Построим кривую, описывающую наше решение
x <- (1:100) / 100
test = data.frame(x, x^2, x^3, x^4, x^5)
names(test) <- c('X', 'X2', 'X3', 'X4', 'X5')
y0 <- predict(simple, test)

plot(X, Y, ylim = range(y0), xlim = c(0,1))
lines(x, y0, col = 'red')