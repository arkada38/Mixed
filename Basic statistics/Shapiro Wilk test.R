# Shapiro–Wilk test

library(datasets)
head(iris)

hist(iris$Sepal.Width)

# H0: распределение нормальное
# H1: распределение не нормальное

shapiro.test(iris$Sepal.Width)
# Shapiro-Wilk normality test
# 
# data:  iris$Sepal.Width
# W = 0.98492, p-value = 0.1012

# α = 0.05
# p > α (Accept null hypothesis)
# Вероятность получить такое же или более экстремальное распределение
# составляет 0.1012, что выше нашего уровня значимости.

# Распределение данных не существенно отиличается от нормального.
# Гипотеза о нормальности распределения не отвергается.